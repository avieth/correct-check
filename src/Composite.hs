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
  ( -- * Declaring tests
    Declaration
  , composite
  , declare
  , compose

    -- * Composing tests
  , Composite
  , effect
  , effect_
  , check
  , assert
  , stop
  , StopTestEarly (..)

    -- * Showing results
  , TestResult (..)
  , printTestResult

    -- * Configuration
  , LocalConfig (..)
  , defaultLocalConfig
  , serially
  , inParallel
  , GlobalConfig (..)
  , defaultGlobalConfig
  , Parallelism (..)
  , noParallelism
  , nCapabilities

    -- * Re-export
  , Natural
  ) where

import Control.Concurrent.STM hiding (check)
import Control.Exception (SomeException, Exception, evaluate, throwIO, try)
import Control.Monad (ap, unless)
import qualified Control.Monad.IO.Class as Lift
import qualified Control.Monad.IO.Unlift as Unlift
import Check
import Numeric.Natural (Natural)
import Location
import Pretty
import Types
import Space.Random as Random
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty as NE (NonEmpty, nonEmpty)
import Data.Word (Word32)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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

-- | Counterexample with extra information about where it came from.
data FailingCase where
  FailingCase :: String
              -> MaybeSrcLoc
              -- ^ Of the declaration
              -> MaybeSrcLoc
              -- ^ Of the check
              -> RenderTest specimen result assertion
              -> RenderDomain space
              -> Counterexample space specimen result assertion
              -> FailingCase

-- | Include a test counterexample in the state.
addCounterexample :: String -- ^ Name of declaration
                  -> MaybeSrcLoc -- ^ Of declaration site
                  -> MaybeSrcLoc -- ^ Of check site
                  -> RenderTest specimen result assertion
                  -> RenderDomain space
                  -> TVar CheckState
                  -> Counterexample space specimen result assertion
                  -> STM ()
addCounterexample name declSrcLoc checkSrcLoc renderTest renderDomain tvar !cx = do
  st <- readTVar tvar
  writeTVar tvar $! st
    { checkStateFailingTests =
               checkStateFailingTests st
        Seq.|> FailingCase name declSrcLoc checkSrcLoc renderTest renderDomain cx
    }

-- | Inlcude a test result. No change if it's a pass (Nothing).
addPropertyTestResult :: String -- ^ Name of declaration
                      -> MaybeSrcLoc -- ^ Of declaration site
                      -> MaybeSrcLoc -- ^ Of check site
                      -> RenderTest specimen result assertion
                      -> RenderDomain space
                      -> TVar CheckState
                      -> Maybe (Counterexample space specimen result assertion)
                      -> STM ()
addPropertyTestResult name declSrcLoc checkSrcLoc renderTest renderDomain tvar =
  maybe (pure ()) (addCounterexample name declSrcLoc checkSrcLoc renderTest renderDomain tvar)

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
  $ [fromString "Ended with exception:", PP.viaShow ex ]

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
      Just neFailingCases -> PP.line <> ppFailingCases neFailingCases

ppFailingCases :: NonEmpty FailingCase -> Doc AnsiStyle
ppFailingCases = mconcat . intersperse PP.line . fmap ppFailingCase . toList

ppFailingCase :: FailingCase -> Doc AnsiStyle
ppFailingCase (FailingCase name declSrcLoc checkSrcLoc renderTest renderDomain cexample) = PP.vcat
  [ PP.annotate PP.Ansi.bold (PP.annotate (PP.Ansi.color PP.Ansi.Red) (fromString "✘" PP.<+> fromString name))
  , PP.indent 2 $ PP.vsep
    [ PP.hsep
        [ boldString "Declared at  "
        , PP.annotate (PP.Ansi.color PP.Ansi.Cyan) (prettyMaybeSrcLoc declSrcLoc)
        ]
    , PP.hsep
        [ boldString "Checked at   "
        , PP.annotate (PP.Ansi.color PP.Ansi.Cyan) (prettyMaybeSrcLoc checkSrcLoc)
        ]
    {-
    -- TODO decide how to show a domain, if at all.
    , PP.hsep
        [ boldString "Over domain  "
        , PP.annotate (PP.Ansi.color PP.Ansi.Cyan) (prettyMaybeSrcLoc (domainSrcLoc dom))
        ]
    -}
    , PP.indent 14 $ PP.vsep
        [ PP.hang 2 $ PP.hsep
          [ fromString "with random seed"
          , PP.annotate (PP.Ansi.color PP.Ansi.Green) (fromString (showSeedHex (randomSeed cexample)))
          ]
        , PP.hang 2 $ PP.hsep
          [ fromString "and search part "
          , PP.nest 2 $ PP.annotate (PP.Ansi.color PP.Ansi.Yellow) (ppMaybe (renderSpace renderDomain) (searchPoint cexample))
          ]
        ]
    , PP.hsep
        [ boldString "Specimen     "
        , PP.nest 2 $ PP.annotate (PP.Ansi.color PP.Ansi.Green) (ppMaybe (renderSpecimen renderTest) (generatedValue cexample))
        ]
    , PP.hsep
        [ boldString "Result       "
        , PP.nest 2 $ PP.annotate (PP.Ansi.color PP.Ansi.Blue) (ppMaybe (renderResult renderTest) (testResult cexample))
        ]
    , boldString "Refuting " PP.<+> PP.indent 4 (ppAssertions renderTest (refutations cexample))
    ]
  ]
  where boldString = PP.annotate PP.Ansi.bold . fromString

ppAssertions :: RenderTest specimen result assertion
             -> NonEmpty (Assertion assertion)
             -> Doc AnsiStyle
ppAssertions renderTest assertions = PP.vsep (fmap (ppAssertion renderTest) (toList assertions))

ppAssertion :: RenderTest specimen result assertion
            -> Assertion assertion
            -> Doc AnsiStyle
ppAssertion renderTest assertion = PP.hsep
  [ PP.annotate (PP.Ansi.color PP.Ansi.Red) (ppMaybe (renderAssertion renderTest) (assertionLabel assertion))
  , fromString "at"
  , PP.annotate (PP.Ansi.color PP.Ansi.Cyan) (prettyMaybeSrcLoc (assertionLocation assertion))
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

-- | A composite is able to call some function that will run a test over a
-- given domain and static point.
--
-- The check gives a bool saying whether it passed, which allows for a
-- composite to choose to exit early.
--
-- The parameter `check` will be instantiated at `Check`, but will appear
-- universally quantified during construction.
newtype Composite check t = Composite
  { runComposite ::
         (forall space specimen .
              MaybeSrcLoc -- Of the call site to this check in the composite
           -> LocalConfig
           -> RenderDomain space
           -> check specimen
           -> Domain space specimen -- The domain to use
           -> IO Bool
         )
      -> IO t
  }

instance Functor (Composite property) where
  fmap f (Composite k) = Composite (\l -> fmap f (k l))

instance Applicative (Composite property) where
  pure x = Composite (\_ -> pure x)
  (<*>) = ap

instance Monad (Composite property) where
  return = pure
  Composite left >>= k = Composite (\l -> left l >>= runCompositeAt l . k)

-- MonadIO and MonadUnliftIO are given, to make it easier to write composites:
-- there's already a wide variety of "lifted" variants of things out there.

instance Lift.MonadIO (Composite property) where
  {-# INLINE liftIO #-}
  liftIO io = Composite $ \_ -> io

instance Unlift.MonadUnliftIO (Composite property) where
  {-# INLINE withRunInIO #-}
  withRunInIO k = Composite $ \l -> k (runCompositeAt l)

{-# INLINE effect #-}
effect :: ((forall r . Composite property r -> IO r) -> IO t) -> Composite property t
effect = Unlift.withRunInIO

{-# INLINE effect_ #-}
effect_ :: IO t -> Composite property t
effect_ = Lift.liftIO

-- | Flipped 'runComposite'.
{-# INLINE runCompositeAt #-}
runCompositeAt ::
     (forall space specimen .
          MaybeSrcLoc
       -> LocalConfig
       -> RenderDomain space
       -> check specimen
       -> Domain space specimen
       -> IO Bool
     )
  -> Composite check t
  -> IO t
runCompositeAt k comp = runComposite comp k

-- | The `check` parameter of 'Composite' will be instantiated to this in
-- order to run a composite test.
--
-- There is the static environment, the global configuration, both of which come
-- from the driver and not from the local test declaration.
--
-- The MaybeSrcLoc is the place where the check was called in the composite (not
-- where it was declared).
newtype Check specimen = Check
  { runCheck :: forall space .
                Env
             -> GlobalConfig
             -> MaybeSrcLoc
             -> TVar CheckState
             -> LocalConfig
             -> RenderDomain space
             -> Domain space specimen
             -> IO Bool
  }

{-# INLINE runCompositeCheck #-}
runCompositeCheck :: Env -> GlobalConfig -> TVar CheckState -> Composite Check t -> IO t
runCompositeCheck env gconf tvar (Composite k) =
  k (\srcLoc lconf renderDomain (Check f) -> f env gconf srcLoc tvar lconf renderDomain)

-- | This will check a property within a composite. It will force the evaluation
-- of the property up to the determination of whether it passes or not.
{-# INLINE check #-}
check :: HasCallStack
      => LocalConfig
      -> RenderDomain space
      -> check specimen
      -> Domain space specimen
      -> Composite check Bool
check lconf render prop domain = Composite $ \k ->
  k (srcLocOf callStack) lconf render prop domain >>= evaluate

-- | 'check' but stops the test if it's a failure.
{-# INLINE assert #-}
assert :: HasCallStack
       => LocalConfig
      -> RenderDomain space
       -> check specimen
       -> Domain space specimen
       -> Composite check ()
assert lconf render prop domain = withFrozenCallStack (do
  b <- check lconf render prop domain
  -- FIXME would be better to give an exception that shows it was an
  -- assertion that caused the early exit?
  unless b (stopWithExplanation "assertion failed"))

-- | Used to restrict the form of composite tests. The function 'composite'
-- will eliminate this constructor, but requires that the type parameter
-- `check` be universally quanitified (similar to the ST trick).
newtype Declaration check = Declaration
  { runDeclaration ::
         (forall specimen .
           Check specimen -> check specimen
         )
      -> Composite check ()
  }

-- INLINEs on compose, declare, and composite are very important. Without it,
-- GHC won't be able to simplify unit tests in composites.

{-# INLINE CONLIKE compose #-}
compose :: Composite check () -> Declaration check
compose comp = Declaration (\_ -> comp)

-- | Declare a property, to allow for its use in a composite.
--
-- The test must be inside a StaticPtr. That's key if we want reproducibility.
-- This is the only place where it is enforced; it is `deRefStaticPtr`d here.
{-# INLINE CONLIKE declare #-}
declare :: HasCallStack
        => RenderTest specimen result assertion
        -> String
        -> StaticPtr (Test assertion specimen result)
        -> (check specimen -> Declaration check)
        -> Declaration check
declare renderTest name test k = Declaration $ \toCheck ->
  runDeclaration (k (toCheck (Check (checkOne name (srcLocOf callStack) (deRefStaticPtr test) renderTest)))) toCheck

-- | Takes the source location of the declaration, and also of the call to
-- check/assert.
{-# INLINE checkOne #-}
checkOne :: String -- ^ Name of the declaration
         -> MaybeSrcLoc -- ^ Location of the declaration
         -> Test assertion specimen result
         -> RenderTest specimen result assertion
         -> Env
         -> GlobalConfig
         -> MaybeSrcLoc -- ^ Location of the call to check within the Composite
         -> TVar CheckState
         -> LocalConfig
         -> RenderDomain space
         -> Domain space specimen
         -> IO Bool
checkOne name declSrcLoc test renderTest env gconf checkSrcLoc tvar lconf renderDomain domain = do
  seed <- atomically (splitStateSeed tvar)
  -- FIXME if the config asks for 0 random samples, we should quit. Just like
  -- the quickCheck driver.
  let n = max 1 (clampToWord32 (showMaybeSrcLoc declSrcLoc) (nsamples gconf lconf))
      mp = fmap (clampToWord32 (showMaybeSrcLoc declSrcLoc)) (parallelism env gconf lconf)
      result = case mp of
        Nothing -> checkSequential (randomPoints (n - 1) seed) domain test
        Just m -> checkParallel m (randomPoints (n - 1) seed) domain test
  atomically (addPropertyTestResult name declSrcLoc checkSrcLoc renderTest renderDomain tvar result)
  pure $! isNothing result

-- | Run a composite. Note the ST-style trick: only composites which do not
-- know anythig about the `property` type may be run.
{-# INLINE composite #-}
composite :: GlobalConfig -> (forall check . Declaration check) -> IO TestResult
composite gconf decl = do
  env <- mkEnv
  initialSeed <- Random.newSeedIO
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
data StopTestEarly = StopTestEarly String MaybeSrcLoc

instance Show StopTestEarly where
  show (StopTestEarly str mloc) = mconcat
    [ str, " at ", showMaybeSrcLoc mloc ]

instance Exception StopTestEarly

{-# INLINE stop #-}
stop :: HasCallStack => Composite check x
stop = Composite $ \_ -> throwIO (StopTestEarly "stop test" (srcLocOf callStack))

{-# INLINE stopWithExplanation #-}
stopWithExplanation :: HasCallStack => String -> Composite check x
stopWithExplanation str = Composite $ \_ -> throwIO (StopTestEarly str (srcLocOf callStack))

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
defaultGlobalConfig = GlobalConfig
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

serially :: Natural -> LocalConfig
serially n = LocalConfig
  { localParallelism = noParallelism
  , localRandomSamples = n
  }

-- | Use this many samples, in parallel using the number of capabilities.
inParallel :: Natural -> LocalConfig
inParallel n = LocalConfig
  { localParallelism = nCapabilities
  , localRandomSamples = n
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

noParallelism :: Parallelism
noParallelism = NoParallelism

nCapabilities :: Parallelism
nCapabilities = DynamicParallelism id
