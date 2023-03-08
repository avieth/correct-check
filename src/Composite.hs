-- |
-- = Composite property tests
--
-- When it comes to defining tests which may do IO, there's no point in trying
-- to make the whole thing reproducible. Instead, we can define tests for
-- side-effecting programs by composing pure property tests within IO, such that
-- the static parameters of these properties may come from that IO, but the
-- properties themselves are known statically without IO.
--
-- If we can show details of a counterexample on a terminal, and possibly in
-- some machine-readable binary format, then we can reproduce every failing pure
-- property in a failing composite IO property, even though the composite cannot
-- itself be reproduced.
--
-- Pretty printing is defined in this module because a composite property isn't
-- useful without it.

module Composite
  ( Declaration
  , composite
  , declare
  , compose

  , Composite
  , effect
  , bracket
  , check
  , assert
  , stop
  , Check
  , StopTestEarly (..)

  , TestResult (..)
  , printTestResult

  , Renderer (..)
  , noRenderer
  , viaShowRenderer

  , LocalConfig (..)
  , GlobalConfig (..)
  , Parallelism (..)
  , nCapabilities
  , defaultGlobalConfig
  , defaultLocalConfig
  ) where

import Control.Concurrent.STM hiding (check)
import Control.Exception (SomeException, Exception, evaluate, throwIO, try)
import qualified Control.Exception as Exception
import Control.Monad (ap, unless)
import Location
import Property
import Types
import Space.Random as Random
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty as NE (NonEmpty, nonEmpty)
import Data.Word (Word32)
import Driver (checkParallel, checkSequential)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (fromString)
import qualified GHC.RTS.Flags as RTS (nCapabilities, getParFlags)
import Prettyprinter (Doc)
import qualified Prettyprinter as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as PP.Ansi

-- | Will be held in a TVar for use in a composite property run.
--
-- Individual properties will use this to get a new seed by splitting the one
-- here. If the composite does not use any concurrency or otherwise introduce
-- non-determinism, then the tests will get deterministic seeds; if not, they
-- won't, but it doesn't matter because in this case the composite test is
-- non-deterministic anyway. Failing tests will still come with enough
-- information to reproduce them on their own, but we don't expect a composite
-- in general to be reproducible, since it's IO.
data CheckState = CheckState
  { checkStateSeed :: !Seed
  , checkStateFailingTests :: !(Seq FailingCase)
  }

-- | Creates a new TVar for the state.
initialCheckStateIO :: Seed -> IO (TVar CheckState)
initialCheckStateIO seed = newTVarIO $ CheckState
  { checkStateSeed = seed
  , checkStateFailingTests = mempty
  }

-- | Split the seed, update it with one half, and return the other half.
splitStateSeed :: TVar CheckState -> STM Seed
splitStateSeed tvar = do
  st <- readTVar tvar
  let (s1, s2) = Random.splitTuple (checkStateSeed st)
  writeTVar tvar $! st { checkStateSeed = s2 }
  pure s1

-- | Include a test counterexample in the state.
addCounterexample :: String -- ^ Name of declaration
                  -> MaybeSrcLoc -- ^ Of declaration site
                  -> MaybeSrcLoc -- ^ Of check site
                  -> Renderer state space dynamic result refutation static
                  -> TVar CheckState
                  -> static
                  -> Counterexample space dynamic result refutation
                  -> STM ()
addCounterexample name declSrcLoc checkSrcLoc renderer tvar t !cx = do
  st <- readTVar tvar
  writeTVar tvar $! st
    { checkStateFailingTests =
               checkStateFailingTests st
        Seq.|> FailingCase name declSrcLoc checkSrcLoc renderer t cx
    }

-- | Inlcude a test result. No change if it's a pass (Nothing).
addPropertyTestResult :: String -- ^ Name of declaration
                      -> MaybeSrcLoc -- ^ Of declaration site
                      -> MaybeSrcLoc -- ^ Of check site
                      -> Renderer state space dynamic result refutation static
                      -> TVar CheckState
                      -> static
                      -> Maybe (Counterexample space dynamic result refutation)
                      -> STM ()
addPropertyTestResult name declSrcLoc checkSrcLoc renderer tvar t =
  maybe (pure ()) (addCounterexample name declSrcLoc checkSrcLoc renderer tvar t)

-- | Well get this as the result of a composite test run.
--
-- The initial seed, the final check state, and an exception if there was one.
data TestResult where
  Normal :: Seed -> CheckState -> TestResult
  Exceptional :: Seed -> CheckState -> SomeException -> TestResult

-- | To stdout.
printDoc :: Doc AnsiStyle -> IO ()
printDoc = PP.Ansi.putDoc

printTestResult :: TestResult -> IO ()
printTestResult = printDoc . ppTestResult

ppTestResult :: TestResult -> Doc AnsiStyle
ppTestResult tresult = case tresult of
  Normal seed state -> PP.vsep
    [ ppSummary seed state
    , ppNormalFooter
    , PP.line
    ]
  Exceptional seed state ex -> PP.vsep
    [ ppSummary seed state
    , ppExceptionalFooter ex
    , PP.line
    ]

ppNormalFooter :: Doc AnsiStyle
ppNormalFooter = PP.annotate PP.Ansi.bold (fromString "Ended normally")

ppExceptionalFooter :: SomeException -> Doc AnsiStyle
ppExceptionalFooter ex =
    PP.annotate PP.Ansi.bold
  . PP.annotate (PP.Ansi.color PP.Ansi.Red)
  . PP.hsep
  $ [fromString "Ended with exception", PP.viaShow ex ]

ppSummary :: Seed -> CheckState -> Doc AnsiStyle
ppSummary initialSeed st = PP.annotate PP.Ansi.bold (PP.hsep
  [ fromString "There"
  , fromString verb
  , PP.viaShow (Seq.length (checkStateFailingTests st))
  , fromString noun
  , fromString "discovered from initial random seed"
  , PP.annotate (PP.Ansi.color PP.Ansi.Green) (fromString (showSeedHex initialSeed))
  ]) <> failingSummary
  where
    n = Seq.length (checkStateFailingTests st)
    (verb, noun) = if n == 1 then ("was", "counterexample") else ("were", "counterexamples")
    failingSummary = case NE.nonEmpty (toList (checkStateFailingTests st)) of
      Nothing -> mempty
      Just neFailingCases -> PP.line <> PP.indent 2 (ppFailingCases neFailingCases)

ppFailingCases :: NonEmpty FailingCase -> Doc AnsiStyle
ppFailingCases = mconcat . intersperse PP.line . fmap ppFailingCase . toList

ppFailingCase :: FailingCase -> Doc AnsiStyle
ppFailingCase (FailingCase name declSrcLoc checkSrcLoc renderer staticPart cexample) = PP.vcat
  [ PP.annotate PP.Ansi.bold (PP.annotate (PP.Ansi.color PP.Ansi.Red) (fromString name))
  , PP.indent 2 $ PP.vsep
    [ fromString "Declared at" PP.<+> PP.annotate (PP.Ansi.color PP.Ansi.Cyan) (prettyMaybeSrcLoc declSrcLoc)
    , PP.hsep
        [ fromString "Checked at "
        , PP.annotate (PP.Ansi.color PP.Ansi.Cyan) (prettyMaybeSrcLoc checkSrcLoc)
        ]
    , PP.indent 12 $ PP.hsep
        [ fromString "with static part"
        , PP.annotate (PP.Ansi.color PP.Ansi.Magenta) (PP.nest 2 (ppMaybe (renderStatic renderer) staticPart))
        ]
    , fromString "Refuted" PP.<+> PP.indent 4 (ppCounterexample renderer cexample)
      -- TODO Under here show the space, dynamic part, random seed, and list of
      -- refutations
    ]
  ]

ppCounterexample :: Renderer state space dynamic result refutation static
                 -> Counterexample space dynamic result refutation
                 -> Doc AnsiStyle
ppCounterexample renderer cexample = PP.vsep
  [ PP.vsep (fmap (ppRefutation renderer) (toList (refutations cexample)))
  , PP.hsep
    [ fromString "with random seed"
    , PP.annotate (PP.Ansi.color PP.Ansi.Green) (fromString (showSeedHex (randomSeed cexample)))
    ]
  , PP.hsep
    [ fromString "and search part"
    , PP.hang 2 $ PP.annotate (PP.Ansi.color PP.Ansi.Magenta) (ppMaybe (renderSpace renderer) (searchPoint cexample))
    ]
  , PP.hsep
    [ fromString "generating dynamic part"
    , PP.hang 2 $ PP.annotate (PP.Ansi.color PP.Ansi.Green) (ppMaybe (renderDynamic renderer) (dynamicPart cexample))
    ]
  , PP.hsep
    [ fromString "yielding result"
    , PP.hang 2 $ PP.annotate (PP.Ansi.color PP.Ansi.Blue) (ppMaybe (renderResult renderer) (resultPart cexample))
    ]
  ]

ppRefutation :: Renderer state space dynamic result refutation static
             -> Refutation refutation
             -> Doc AnsiStyle
ppRefutation renderer refutation = PP.hsep
  [ PP.annotate (PP.Ansi.color PP.Ansi.Red) (ppMaybe (renderRefutation renderer) (refutationLabel refutation))
  , fromString "at"
  , PP.annotate (PP.Ansi.color PP.Ansi.Cyan) (prettyMaybeSrcLoc (refutationLocation refutation))
  ]

  {-
ppIntercalateLines :: [Doc AnsiStyle] -> Doc AnsiStyle
ppIntercalateLines = mconcat . intersperse line
  where
    line :: Doc AnsiStyle
    line = PP.line <> fromString "-------------------------" <> PP.line
-}

ppMaybe :: Maybe (t -> Doc ann) -> t -> Doc ann
ppMaybe mPp t = case mPp of
  Nothing -> fromString "< cannot print  >"
  Just k -> k t

-- | A composite is able to call some function that will check a property at a
-- given point. The check gives a bool saying whether it passed, which allows
-- for a composite to choose to exit early.
newtype Composite property t = Composite
  { runComposite :: (forall x . MaybeSrcLoc -> property x -> x -> IO Bool) -> IO t }

instance Functor (Composite property) where
  fmap f (Composite k) = Composite (\l -> fmap f (k l))

instance Applicative (Composite property) where
  pure x = Composite (\_ -> pure x)
  (<*>) = ap

instance Monad (Composite property) where
  return = pure
  Composite left >>= k = Composite (\l -> left l >>= runCompositeAt l . k)

effect :: IO t -> Composite property t
effect io = Composite $ \_ -> io

bracket :: IO r -> (r -> IO ()) -> (r -> Composite property t) -> Composite property t
bracket acquire release k = Composite $ \l -> Exception.bracket acquire release (runCompositeAt l . k)

-- TODO would probably want to give MonadIO and UnliftIO instances, for maximum
-- compatibility.

-- | Flipped 'runComposite'.
{-# INLINE runCompositeAt #-}
runCompositeAt :: (forall x . MaybeSrcLoc -> property x -> x -> IO Bool) -> Composite property t -> IO t
runCompositeAt k comp = runComposite comp k

-- | The `property` parameter of 'Composite' will be instantiated to this in
-- order to run a composite test.
--
-- There is the static environment, the global configuration, both of which come
-- from the driver and not from the local property declaration.
-- The MaybeSrcLoc is the place where the check was called in the composite (not
-- where it was declared).
newtype Check t = Check (Env -> GlobalConfig -> MaybeSrcLoc -> TVar CheckState -> t -> IO Bool)

{-# INLINE runCompositeCheck #-}
runCompositeCheck :: Env -> GlobalConfig -> TVar CheckState -> Composite Check t -> IO t
runCompositeCheck env gconf tvar (Composite k) = k (\srcLoc (Check f) -> f env gconf srcLoc tvar)

-- | This will check a property within a composite. It will force the evaluation
-- of the property up to the determination of whether it passes or not.
check :: HasCallStack => property x -> x -> Composite property Bool
check prop x = Composite $ \k -> k (srcLocOf callStack) prop x >>= evaluate

-- | 'check' but stops the test if it's a failure.
assert :: HasCallStack => property x -> x -> Composite property ()
assert prop x = do
  b <- withFrozenCallStack (check prop x)
  -- FIXME would be better to give an exception that shows it was an
  -- assertion that caused the early exit?
  unless b stop

-- | Used to restrict the form of composite tests. The function 'composite'
-- will eliminate this constructor, but requires that the type parameter
-- `property` be universally quanitified (similar to the ST trick).
newtype Declaration property = Declaration
  { runDeclaration :: (forall r . Check r -> property r) -> Composite property () }

-- INLINEs on compose, declare, and composite are very important. Without it,
-- GHC won't be able to simplify unit tests in composites.

{-# INLINE CONLIKE compose #-}
compose :: Composite property () -> Declaration property
compose comp = Declaration (\_ -> comp)

-- | Declare a property, to allow for its use in a composite.
{-# INLINE CONLIKE declare #-}
declare :: HasCallStack
        => String
        -> Property state space dynamic result refutation t
        -> Renderer state space dynamic result refutation t
        -> LocalConfig
        -> (property t -> Declaration property)
        -> Declaration property
declare name prop renderer lconf k = Declaration $ \toProperty ->
  runDeclaration (k (toProperty (Check (checkOne name (srcLocOf callStack) prop renderer lconf)))) toProperty

-- | Takes the source location of the declaration, and also of the call to
-- check/assert.
{-# INLINE checkOne #-}
checkOne :: String -- ^ Name of the declaration
         -> MaybeSrcLoc -- ^ Location of the declaration
         -> Property state space dynamic result refutation t
         -> Renderer state space dynamic result refutation t
         -> LocalConfig
         -> Env
         -> GlobalConfig
         -> MaybeSrcLoc -- ^ Location of the call to check within the Composite
         -> TVar CheckState
         -> t
         -> IO Bool
checkOne name declSrcLoc prop renderer lconf env gconf checkSrcLoc tvar t = do
  seed <- atomically (splitStateSeed tvar)
  -- FIXME if the config asks for 0 random samples, we should quit. Just like
  -- the quickCheck driver.
  let n = max 1 (clampToWord32 (showMaybeSrcLoc declSrcLoc) (nsamples gconf lconf))
      mp = fmap (clampToWord32 (showMaybeSrcLoc declSrcLoc)) (parallelism env gconf lconf)
      result = case mp of
        Nothing -> checkSequential t (randomPoints (n - 1) seed) prop
        Just m -> checkParallel m t (randomPoints (n - 1) seed) prop
  atomically (addPropertyTestResult name declSrcLoc checkSrcLoc renderer tvar t result)
  pure $! isNothing result

-- | Run a composite. Note the ST-style trick: only composites which do not
-- know anythig about the `property` type may be run.
{-# INLINE composite #-}
composite :: GlobalConfig -> (forall property. Declaration property) -> IO TestResult
composite gconf decl = do
  env <- mkEnv
  initialSeed <- Random.newSMGen
  tvar <- initialCheckStateIO initialSeed
  outcome <- try (runCompositeCheck env gconf tvar (runDeclaration decl id))
  finalState <- readTVarIO tvar
  pure $ case outcome of
    Left someException -> Exceptional initialSeed finalState someException
    Right () -> Normal initialSeed finalState

-- | A test will run all properties, even if some property has failed. It's
-- possible to stop the property test eary by throwing an exception. The check
-- state is in a TVar that can be read after the exception is caught by the
-- test runner.
data StopTestEarly = StopTestEarly

deriving instance Show StopTestEarly
instance Exception StopTestEarly

{-# INLINE stop #-}
stop :: Composite property x
stop = Composite $ \_ -> throwIO StopTestEarly

-- | Counterexample with extra information about where it came from.
data FailingCase where
  FailingCase :: String
              -> MaybeSrcLoc
              -- ^ Of the declaration
              -> MaybeSrcLoc
              -- ^ Of the check
              -> Renderer state space dynamic result refutation static
              -> static
              -> Counterexample space dynamic result refutation
              -> FailingCase

-- | Rendering information about the types of a property, useful for showing
-- diagnostic information.
data Renderer state space specimen result refutation static = Renderer
  { renderState :: forall ann . Maybe (state -> Doc ann)
  , renderSpace :: forall ann . Maybe (space -> Doc ann)
  , renderDynamic :: forall ann . Maybe (specimen -> Doc ann)
  , renderResult :: forall ann . Maybe (result -> Doc ann)
  , renderRefutation :: forall ann . Maybe (refutation -> Doc ann)
  , renderStatic :: forall ann . Maybe (static -> Doc ann)
  }

noRenderer :: Renderer state space dynamic result refutation static
noRenderer = Renderer
  { renderState = Nothing
  , renderSpace = Nothing
  , renderDynamic = Nothing
  , renderResult = Nothing
  , renderRefutation = Nothing
  , renderStatic = Nothing
  }

viaShowRenderer :: (Show state, Show space, Show dynamic, Show result, Show refutation, Show static)
                  => Renderer state space dynamic result refutation static
viaShowRenderer = Renderer
  { renderState = Just tshow
  , renderSpace = Just tshow
  , renderDynamic = Just tshow
  , renderResult = Just tshow
  , renderRefutation = Just tshow
  , renderStatic = Just tshow
  }
  where
    tshow :: Show t => t -> Doc ann
    tshow = PP.viaShow

data Env = Env
  { envCapabilities :: !Word32
  }

mkEnv :: IO Env
mkEnv =  do
  parFlags <- RTS.getParFlags
  pure $ Env
    { envCapabilities = RTS.nCapabilities parFlags
    }

-- | Configuration of a composite run overall.
data GlobalConfig = GlobalConfig
  { globalMaximumParallelism :: Maybe Parallelism
  , globalMaximumRandomSamples :: Maybe Natural
  }

defaultGlobalConfig :: GlobalConfig
defaultGlobalConfig= GlobalConfig
  { globalMaximumParallelism = Nothing
  , globalMaximumRandomSamples = Nothing
  }

-- | Configuration for a particular property.
data LocalConfig = LocalConfig
  { localParallelism :: Parallelism -- ^ How much paralleism?
  , localRandomSamples :: Natural -- ^ How many random samples for this property?
  }

defaultLocalConfig :: LocalConfig
defaultLocalConfig = LocalConfig
  { localParallelism = NoParallelism
  , localRandomSamples = 64
  }

nsamples :: GlobalConfig -> LocalConfig -> Natural
nsamples gconf lconf = case globalMaximumRandomSamples gconf of
  Nothing -> localRandomSamples lconf
  Just n -> min n (localRandomSamples lconf)

parallelismInEnv :: Env -> Parallelism -> Maybe Natural
parallelismInEnv env prl = case prl of
  NoParallelism -> Nothing
  ConstantParallelism n -> Just n
  DynamicParallelism k -> Just (k (fromIntegral (envCapabilities env)))

parallelism :: Env -> GlobalConfig -> LocalConfig -> Maybe Natural
parallelism env gconf lconf = do
  requested <- parallelismInEnv env (localParallelism lconf)
  Just $ case globalMaximumParallelism gconf >>= parallelismInEnv env of
    Nothing -> requested
    Just limited -> min requested limited

data Parallelism where
  NoParallelism :: Parallelism
  ConstantParallelism :: Natural -> Parallelism
  -- | Given the number of capabilities, decide the amount of parallelism.
  DynamicParallelism :: (Natural -> Natural) -> Parallelism

nCapabilities :: Parallelism
nCapabilities = DynamicParallelism id
