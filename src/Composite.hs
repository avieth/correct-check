module Composite
  ( Declaration
  , composite
  , declare

  , Composite
  , effect
  , bracket
  , check
  , assert
  , stop
  , Check
  , StopTestEarly (..)

  , TestResult (..)
  , showTestResult
  , showSummary

  , Metadata (..)
  ) where

import Control.Concurrent.STM hiding (check)
import Control.Exception (SomeException, Exception, evaluate, throwIO, try)
import qualified Control.Exception as Exception
import Control.Monad (ap, unless)
import Data.Text (Text)
import Location
import Property
import Types
import Space.Random as Random
import Driver (Counterexample, checkSequential)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Semigroup (All (..))

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
addCounterexample :: TVar CheckState -> Counterexample space dynamic refutation -> STM ()
addCounterexample tvar cx = do
  st <- readTVar tvar
  writeTVar tvar $! st { checkStateFailingTests = checkStateFailingTests st Seq.|> FailingCase cx }

-- | Inlcude a test result. No change if it's a pass (Nothing).
{-# INLINE addPropertyTestResult #-}
addPropertyTestResult :: TVar CheckState -> Maybe (Counterexample space dynamic refutation) -> STM ()
addPropertyTestResult tvar = maybe (pure ()) (addCounterexample tvar)

-- | Well get this as the result of a composite test run.
--
-- The initial seed, the final check state, and an exception if there was one.
data TestResult where
  Normal :: Seed -> CheckState -> TestResult
  Exceptional :: Seed -> CheckState -> SomeException -> TestResult

showSummary :: Seed -> CheckState -> IO ()
showSummary initialSeed st = do
  putStrLn $ mconcat
    [ "Initial random seed is ", showSeedHex initialSeed, "\n"
    , "There were "
    , show (Seq.length (checkStateFailingTests st))
    , " counterexamples discovered."
    ]

showNormalFooter :: IO ()
showNormalFooter = putStrLn $ mconcat
  [ "Ended normally"
  ]

showExceptionalFooter :: SomeException -> IO ()
showExceptionalFooter ex = putStrLn $ mconcat
  [ "Ended exceptionally\n"
  , show ex
  ]

showTestResult :: TestResult -> IO ()
showTestResult (Normal initialSeed finalState) = do
  showSummary initialSeed finalState
  showNormalFooter
showTestResult (Exceptional initialSeed finalState ex) = do
  showSummary initialSeed finalState
  showExceptionalFooter ex

-- | A composite is able to call some function that will check a property at a
-- given point. The check gives a bool saying whether it passed, which allows
-- for a composite to choose to exit early.
newtype Composite property t = Composite
  { runComposite :: (forall x . property x -> x -> IO Bool) -> IO t }

instance Functor (Composite property) where
  {-# INLINE fmap #-}
  fmap f (Composite k) = Composite (\l -> fmap f (k l))

instance Applicative (Composite property) where
  {-# INLINE pure #-}
  pure x = Composite (\_ -> pure x)
  (<*>) = ap

instance Monad (Composite property) where
  return = pure
  {-# INLINE (>>=) #-}
  Composite left >>= k = Composite (\l -> left l >>= runCompositeAt l . k)

{-# INLINE effect #-}
effect :: IO t -> Composite property t
effect io = Composite $ \_ -> io

{-# INLINE bracket #-}
bracket :: IO r -> (r -> IO ()) -> (r -> Composite property t) -> Composite property t
bracket acquire release k = Composite $ \l -> Exception.bracket acquire release (runCompositeAt l . k)

-- TODO would probably want to give MonadIO and UnliftIO instances, for maximum
-- compatibility.

-- | Flipped 'runComposite'.
{-# INLINE runCompositeAt #-}
runCompositeAt :: (forall x . property x -> x -> IO Bool) -> Composite property t -> IO t
runCompositeAt k comp = runComposite comp k

-- | The `property` parameter of 'Composite' will be instantiated to this in
-- order to run a composite test.
newtype Check t = Check (TVar CheckState -> t -> IO Bool)

{-# INLINE runCompositeCheck #-}
runCompositeCheck :: TVar CheckState -> Composite Check t -> IO t
runCompositeCheck tvar (Composite k) = k (\(Check f) -> f tvar)

-- | This will check a property within a composite. It will force the evaluation
-- of the property up to the determination of whether it passes or not.
{-# INLINE check #-}
check :: property x -> x -> Composite property Bool
check prop x = Composite $ \k -> k prop x >>= evaluate
-- TODO future direction here: take the SrcLoc of the check call, and pass
-- it to the property evaluation function to allow for it to be stored in the
-- test results.

-- | 'check' but stops the test if it's a failure.
{-# INLINE assert #-}
assert :: property x -> x -> Composite property ()
assert prop x = do
  b <- check prop x
  unless b stop

-- TODO
-- Would actually require some config options, like parallelism, number of
-- tests, etc.

-- How about this style? CPS and no existential types.
-- Also, the property is not in conjunction, you can only give one at a time.
-- In this style of definition, that makes more sense anyway.
newtype Declaration property = Declaration
  { runDeclaration :: forall r .
         (forall state space dynamic result refutation t . Property state space dynamic result refutation t -> (property t -> Composite property ()) -> Composite property r)
      -> Composite property r
  }

-- | Declare a property, to allow for its use in a composite.
{-# INLINE declare #-}
declare :: Property state space dynamic result refutation t -> (property t -> Composite property ()) -> Declaration property
declare conj k = Declaration $ \f -> f conj k

-- | Run a composite. Note the ST-style trick: only composites which do not
-- know anythig about the `property` type may be run.
{-# INLINE composite #-}
composite :: (forall property. Declaration property) -> IO TestResult
composite decl = do
  initialSeed <- Random.newSMGen
  tvar <- initialCheckStateIO initialSeed
  outcome <- try (runCompositeCheck tvar (runDeclaration decl runDeclarationWith))
  finalState <- readTVarIO tvar
  pure $ case outcome of
    Left someException -> Exceptional initialSeed finalState someException
    Right () -> Normal initialSeed finalState

{-# INLINE runDeclarationWith #-}
runDeclarationWith :: Property state space dynamic result refutation t -> (Check t -> Composite Check ()) -> Composite Check ()
runDeclarationWith prop k = k (Check (checkOne prop))

{-# INLINE checkOne #-}
checkOne :: Property state space dynamic result refutation t -> TVar CheckState -> t -> IO Bool
checkOne prop tvar t = do
  seed <- atomically (splitStateSeed tvar)
  let result = checkSequential t (randomPoints 99 seed) prop
  atomically (addPropertyTestResult tvar result)
  pure $! isNothing result

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
  FailingCase :: Counterexample space dynamic refutation -> FailingCase

-- | Information about the types of a property, useful for showing diagnostic
-- information.
data Metadata state space specimen result refutation param = Metadata
  { metadataLocation :: MaybeSrcLoc -- Where was the property stated.
  , metadataShowState :: Maybe (state -> Text)
  , metadataShowSpace :: Maybe (space -> Text)
  , metadataShowSpecimen :: Maybe (specimen -> Text)
  , metadataShowResult :: Maybe (result -> Text)
  , metadataShowRefutation :: Maybe (refutation -> Text)
  , metadataShowParam :: Maybe (param -> Text)
  }
