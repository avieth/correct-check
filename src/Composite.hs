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

  , Metadata (..)
  , noMetadata
  , showMetadata

  , LocalConfig (..)
  , GlobalConfig (..)
  , Parallelism (..)
  , defaultGlobalConfig
  , defaultLocalConfig
  ) where

import Control.Concurrent.STM hiding (check)
import Control.Exception (SomeException, Exception, evaluate, throwIO, try)
import qualified Control.Exception as Exception
import Control.Monad (ap, unless)
import Data.Text (Text)
import qualified Data.Text as T
import Location
import Property
import Types
import Space.Random as Random
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Word (Word32)
import Driver (checkParallel, checkSequential)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.RTS.Flags (nCapabilities, getParFlags)

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
{-# INLINE initialCheckStateIO #-}
initialCheckStateIO :: Seed -> IO (TVar CheckState)
initialCheckStateIO seed = newTVarIO $ CheckState
  { checkStateSeed = seed
  , checkStateFailingTests = mempty
  }

-- | Split the seed, update it with one half, and return the other half.
{-# INLINE splitStateSeed #-}
splitStateSeed :: TVar CheckState -> STM Seed
splitStateSeed tvar = do
  st <- readTVar tvar
  let (s1, s2) = Random.split_ (checkStateSeed st)
  writeTVar tvar $! st { checkStateSeed = s2 }
  pure s1

-- | Include a test counterexample in the state.
{-# INLINE addCounterexample #-}
addCounterexample :: MaybeSrcLoc -- ^ Of declaration site
                  -> MaybeSrcLoc -- ^ Of check site
                  -> Metadata state space dynamic result refutation static
                  -> TVar CheckState
                  -> static
                  -> Counterexample space dynamic refutation
                  -> STM ()
addCounterexample declSrcLoc checkSrcLoc meta tvar t !cx = do
  st <- readTVar tvar
  writeTVar tvar $! st
    { checkStateFailingTests =
               checkStateFailingTests st
        Seq.|> FailingCase declSrcLoc checkSrcLoc meta t cx
    }

-- | Inlcude a test result. No change if it's a pass (Nothing).
{-# INLINE addPropertyTestResult #-}
addPropertyTestResult :: MaybeSrcLoc -- ^ Of declaration site
                      -> MaybeSrcLoc -- ^ Of check site
                      -> Metadata state space dynamic result refutation static
                      -> TVar CheckState
                      -> static
                      -> Maybe (Counterexample space dynamic refutation)
                      -> STM ()
addPropertyTestResult declSrcLoc checkSrcLoc meta tvar t =
  -- FIXME use the metadata.
  maybe (pure ()) (addCounterexample declSrcLoc checkSrcLoc meta tvar t)

-- | Well get this as the result of a composite test run.
--
-- The initial seed, the final check state, and an exception if there was one.
data TestResult where
  Normal :: Seed -> CheckState -> TestResult
  Exceptional :: Seed -> CheckState -> SomeException -> TestResult

printSummary :: Seed -> CheckState -> IO ()
printSummary initialSeed st = do
  putStrLn $ mconcat
    [ "Initial random seed is ", showSeedHex initialSeed, "\n"
    , "There ", verb, " "
    , show (Seq.length (checkStateFailingTests st))
    , " ", noun, " discovered"
    , if n == 0 then [] else mconcat ["\n", counterexamples]
    ]
  where
    n = Seq.length (checkStateFailingTests st)
    (verb, noun) = if n == 1 then ("was", "counterexample") else ("were", "counterexamples")
    counterexamples :: String
    counterexamples = intercalate "\n" (fmap showFailingCase (toList (checkStateFailingTests st)))

printNormalFooter :: IO ()
printNormalFooter = putStrLn $ mconcat
  [ "Ended normally" ]

printExceptionalFooter :: SomeException -> IO ()
printExceptionalFooter ex = putStrLn $ mconcat
  [ "Ended with exception ", show ex ]

printTestResult :: TestResult -> IO ()
printTestResult (Normal initialSeed finalState) = do
  printSummary initialSeed finalState
  printNormalFooter
printTestResult (Exceptional initialSeed finalState ex) = do
  printSummary initialSeed finalState
  printExceptionalFooter ex

showFailingCase :: FailingCase -> String
showFailingCase (FailingCase declSrcLoc checkSrcLoc meta staticPart cexample) = mconcat
  [ "Declared at ", prettyMaybeSrcLoc declSrcLoc, "\n"
  , "Checked at ", prettyMaybeSrcLoc checkSrcLoc, "\n"
  , showCounterexample meta staticPart cexample
  ]

showCounterexample :: Metadata state space dynamic result refutation static
                   -> static
                   -> Counterexample space dynamic refutation
                   -> String
showCounterexample meta staticPart cexample = mconcat
  [ "Static part: ", maybeShow (metadataShowStatic meta) staticPart, "\n"
  , "Random seed: ", showSeedHex (randomSeed cexample), "\n"
  , "Dynamic part: ", maybeShow (metadataShowDynamic meta) (dynamicPart cexample), "\n"
  , "Point: ", maybeShow (metadataShowSpace meta) (searchPoint cexample), "\n"
  , "Refuted that: \n"
  , intercalate "\n" (fmap (showRefutation meta) (toList (refutations cexample)))
  ]

showRefutation :: Metadata state space dynamic result refutation static
               -> refutation
               -> String
showRefutation meta = maybeShow (metadataShowRefutation meta)

maybeShow :: Maybe (t -> Text) -> t -> String
maybeShow mshow t = case mshow of
  Nothing -> "< cannot print  >"
  Just k -> T.unpack (k t)


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
{-
bracket acquire release k = Composite $ \l -> Exception.mask $ \restore -> do
  r <- acquire
  t <- restore (runComposite (k r) l) `Exception.onException` release r
  release r
  pure t
-}

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
-- TODO future direction here: take the SrcLoc of the check call, and pass
-- it to the property evaluation function to allow for it to be stored in the
-- test results.

-- | 'check' but stops the test if it's a failure.
assert :: HasCallStack => property x -> x -> Composite property ()
assert prop x = do
  b <- withFrozenCallStack (check prop x)
  -- FIXME would be better to give an exception that shows it was an
  -- assertion that caused the early exit?
  unless b stop

-- TODO
-- Would actually require some config options, like parallelism, number of
-- tests, etc.

-- How about this style? CPS and no existential types.
-- Also, the property is not in conjunction, you can only give one at a time.
-- In this style of definition, that makes more sense anyway.

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
        => Property state space dynamic result refutation t
        -> Metadata state space dynamic result refutation t
        -> LocalConfig
        -> (property t -> Declaration property)
        -> Declaration property
declare prop meta lconf k = Declaration $ \toProperty ->
  runDeclaration (k (toProperty (Check (checkOne (srcLocOf callStack) prop meta lconf)))) toProperty

-- | Takes the source location of the declaration, and also of the call to
-- check/assert.
{-# INLINE checkOne #-}
checkOne :: MaybeSrcLoc
         -> Property state space dynamic result refutation t
         -> Metadata state space dynamic result refutation t
         -> LocalConfig
         -> Env
         -> GlobalConfig
         -> MaybeSrcLoc
         -> TVar CheckState
         -> t
         -> IO Bool
checkOne declSrcLoc prop meta lconf env gconf checkSrcLoc tvar t = do
  seed <- atomically (splitStateSeed tvar)
  -- FIXME if the config asks for 0 random samples, we should quit. Just like
  -- the quickCheck driver.
  let n = max 1 (clampToWord32 (prettyMaybeSrcLoc declSrcLoc) (nsamples gconf lconf))
      mp = fmap (clampToWord32 (prettyMaybeSrcLoc declSrcLoc)) (parallelism env gconf lconf)
      result = case mp of
        Nothing -> checkSequential t (randomPoints (n - 1) seed) prop
        Just m -> checkParallel m t (randomPoints (n - 1) seed) prop
  atomically (addPropertyTestResult declSrcLoc checkSrcLoc meta tvar t result)
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

stop :: Composite property x
stop = Composite $ \_ -> throwIO StopTestEarly

-- | Counterexample with metadata.
-- TODO define the metadata, to allow for communication to human eyes and
-- possibly to machine-readable formats.
data FailingCase where
  FailingCase :: MaybeSrcLoc
              -- ^ Of the declaration
              -> MaybeSrcLoc
              -- ^ Of the check
              -> Metadata state space dynamic result refutation static
              -> static
              -> Counterexample space dynamic refutation
              -> FailingCase

-- | Information about the types of a property, useful for showing diagnostic
-- information.
--
data Metadata state space specimen result refutation static = Metadata
  -- FIXME rename. It's not metadata.
  { metadataShowState :: Maybe (state -> Text)
  , metadataShowSpace :: Maybe (space -> Text)
  , metadataShowDynamic :: Maybe (specimen -> Text)
  , metadataShowResult :: Maybe (result -> Text)
  , metadataShowRefutation :: Maybe (refutation -> Text)
  , metadataShowStatic :: Maybe (static -> Text)
  }

noMetadata :: Metadata state space dynamic result refutation static
noMetadata = Metadata
  { metadataShowState = Nothing
  , metadataShowSpace = Nothing
  , metadataShowDynamic = Nothing
  , metadataShowResult = Nothing
  , metadataShowRefutation = Nothing
  , metadataShowStatic = Nothing
  }

showMetadata :: (Show state, Show space, Show dynamic, Show result, Show refutation, Show static)
             => Metadata state space dynamic result refutation static
showMetadata = Metadata
  { metadataShowState = Just tshow
  , metadataShowSpace = Just tshow
  , metadataShowDynamic = Just tshow
  , metadataShowResult = Just tshow
  , metadataShowRefutation = Just tshow
  , metadataShowStatic = Just tshow
  }
  where
    tshow :: Show t => t -> Text
    tshow = T.pack . show

data Env = Env
  { envCapabilities :: !Word32
  }

mkEnv :: IO Env
mkEnv =  do
  parFlags <- getParFlags
  pure $ Env
    { envCapabilities = nCapabilities parFlags
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
