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
  , checkAsync
  , awaitCheck
  , stop
  , stopBecause
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
  , asynchronousChecks
  , Parallelism (..)
  , noParallelism
  , nCapabilities

    -- * Re-export
  , Natural
  ) where

import Control.Concurrent.Async as Async
import Control.Concurrent.STM hiding (check)
import Control.Concurrent.STM.TSem
import Control.Exception (SomeException (..), Exception, evaluate, throwIO, try)
import qualified Control.Exception as Exception
import Control.Monad (ap)
import qualified Control.Monad.IO.Class as Lift
import qualified Control.Monad.IO.Unlift as Unlift
import Check
import Numeric.Natural (Natural)
import Location
import Pretty
import Types
import Space.Random as Random
import Data.Foldable (toList)
import Data.List.NonEmpty as NE (NonEmpty)
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
  , checkStateTotalChecks :: !Natural
  -- ^ How many checks were made.
  , checkStateFailingTests :: !(Seq FailingCase)
  }

-- | Creates a new TVar for the state.
initialCheckStateIO :: Seed -> IO (TVar CheckState)
initialCheckStateIO seed = newTVarIO $ CheckState
  { checkStateSeed = seed
  , checkStateTotalChecks = 0
  , checkStateFailingTests = mempty
  }

incrementCheckCount :: CheckState -> CheckState
incrementCheckCount cs = cs { checkStateTotalChecks = checkStateTotalChecks cs + 1 }

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
              -> Either SomeException (Counterexample space specimen result assertion)
              -- ^ Even though the test is pure, it could still be partial.
              -> FailingCase

-- | Include a test counterexample in the state.
addCounterexample :: String -- ^ Name of declaration
                  -> MaybeSrcLoc -- ^ Of declaration site
                  -> MaybeSrcLoc -- ^ Of check site
                  -> RenderTest specimen result assertion
                  -> RenderDomain space
                  -> TVar CheckState
                  -> Either SomeException (Counterexample space specimen result assertion)
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
                      -> Either SomeException (Maybe (Counterexample space specimen result assertion))
                      -> STM ()
addPropertyTestResult name declSrcLoc checkSrcLoc renderTest renderDomain tvar =
  maybe (pure ()) (addCounterexample name declSrcLoc checkSrcLoc renderTest renderDomain tvar) . commute

commute :: Either a (Maybe b) -> Maybe (Either a b)
commute (Left err) = Just (Left err)
commute (Right Nothing) = Nothing
commute (Right (Just t)) = Just (Right t)

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
ppSummary initialSeed st = PP.vsep $
  [ PP.annotate PP.Ansi.bold $ PP.hsep
      [ fromString "There"
      , fromString verb
      , PP.viaShow (Seq.length (checkStateFailingTests st))
      , fromString noun
      , fromString "discovered from initial random seed"
      , PP.annotate (PP.Ansi.color PP.Ansi.Green) (fromString (showSeedHex initialSeed))
      ]
  ] ++ failingSummary ++ [
    PP.annotate PP.Ansi.bold $ PP.hsep
      [ fromString "A total of"
      , fromString (show (checkStateTotalChecks st))
      , fromString nounTests
      , fromString verbTests
      , fromString "made"
      ]
  ]
  where
    n = Seq.length (checkStateFailingTests st)
    (verb, noun) = if n == 1 then ("was", "counterexample") else ("were", "counterexamples")
    (verbTests, nounTests) = if checkStateTotalChecks st == 1 then ("was", "check") else ("were", "checks")

    failingSummary = ppFailingCase <$> toList (checkStateFailingTests st)

ppFailingCase :: FailingCase -> Doc AnsiStyle
ppFailingCase (FailingCase name declSrcLoc checkSrcLoc renderTest renderDomain exceptionOrCounterexample) = PP.vcat
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
    , either ppExceptionalFailure ppCounterexample exceptionOrCounterexample
    ]
  ]
  where
    ppCounterexample cexample = PP.vsep
      [ PP.indent 14 $ PP.vsep
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
    ppExceptionalFailure (SomeException ex) = PP.vsep
      [ PP.hsep
          [ boldString "Exception    "
          , PP.nest 2 $ PP.annotate (PP.Ansi.color PP.Ansi.Red) (PP.viaShow ex)
          ]
      ]
    boldString = PP.annotate PP.Ansi.bold . fromString

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
         AsyncCheckEnv
      -> (forall space specimen .
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
  fmap f (Composite k) = Composite (\mq l -> fmap f (k mq l))

instance Applicative (Composite property) where
  pure x = Composite (\_ _ -> pure x)
  (<*>) = ap

instance Monad (Composite property) where
  return = pure
  Composite left >>= k = Composite (\mq l -> left mq l >>= runCompositeAt mq l . k)

-- MonadIO and MonadUnliftIO are given, to make it easier to write composites:
-- there's already a wide variety of "lifted" variants of things out there.

instance Lift.MonadIO (Composite property) where
  {-# INLINE liftIO #-}
  liftIO io = Composite $ \_ _ -> io

instance Unlift.MonadUnliftIO (Composite property) where
  {-# INLINE withRunInIO #-}
  withRunInIO k = Composite $ \mq l -> k (runCompositeAt mq l)

{-# INLINE effect #-}
effect :: ((forall r . Composite property r -> IO r) -> IO t) -> Composite property t
effect = Unlift.withRunInIO

{-# INLINE effect_ #-}
effect_ :: IO t -> Composite property t
effect_ = Lift.liftIO

-- | Flipped 'runComposite'.
{-# INLINE runCompositeAt #-}
runCompositeAt ::
     AsyncCheckEnv
  -> (forall space specimen .
          MaybeSrcLoc
       -> LocalConfig
       -> RenderDomain space
       -> check specimen
       -> Domain space specimen
       -> IO Bool
     )
  -> Composite check t
  -> IO t
runCompositeAt mq k comp = runComposite comp mq k

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
                MaybeSrcLoc
             -> TVar CheckState
             -> ResolvedConfig
             -> RenderDomain space
             -> Domain space specimen
             -> IO Bool
  }

{-# INLINE runCompositeCheck #-}
runCompositeCheck :: Env
                  -> GlobalConfig
                  -> AsyncCheckEnv
                  -> TVar CheckState
                  -> Composite Check t
                  -> IO t
runCompositeCheck env gconf asyncEnv tvar (Composite k) =
  k asyncEnv (\srcLoc lconf renderDomain (Check f) ->
    -- Force the config here, in case it has a value that won't fit in Word32.
    -- In this case we want the composite to fail early.
    let !resolvedConfig = resolveConfig (showMaybeSrcLoc srcLoc) env gconf lconf
     in f srcLoc tvar resolvedConfig renderDomain)

-- | This will check a property within a composite. It will force the evaluation
-- of the property up to the determination of whether it passes or not.
{-# INLINE check #-}
check :: HasCallStack
      => LocalConfig
      -> RenderDomain space
      -> check specimen
      -> Domain space specimen
      -> Composite check Bool
check lconf render check domain = Composite $ \_ k ->
  k (srcLocOf callStack) lconf render check domain

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
declare renderTest name test k = Declaration $ \toCheck -> runDeclaration
  (k (toCheck (Check (checkOne name (srcLocOf callStack) (deRefStaticPtr test) renderTest)))) toCheck

-- | Takes the source location of the declaration, and also of the call to
-- check/assert.
{-# INLINE checkOne #-}
checkOne :: String -- ^ Name of the declaration
         -> MaybeSrcLoc -- ^ Location of the declaration
         -> Test assertion specimen result
         -> RenderTest specimen result assertion
         -> MaybeSrcLoc -- ^ Location of the call to check within the Composite
         -> TVar CheckState
         -> ResolvedConfig
         -> RenderDomain space
         -> Domain space specimen
         -> IO Bool
checkOne name declSrcLoc test renderTest checkSrcLoc tvar rconf renderDomain domain = do
  seed <- atomically (splitStateSeed tvar)
  -- ResolvedConfig should ensure n > 1.
  let n = resolvedRandomSamples rconf
      m = resolvedParallelism rconf
      result = case m > 1 of
        False -> checkSequential (randomPoints (n - 1) seed) domain test
        True -> checkParallel m (randomPoints (n - 1) seed) domain test
  -- The test is pure, but it can still throw an exception (partial match,
  -- error call, etc.).
  outcome <- try (evaluate result)
  atomically $ do
    addPropertyTestResult name declSrcLoc checkSrcLoc renderTest renderDomain tvar outcome
    modifyTVar' tvar incrementCheckCount
  -- Exceptions are considered test failures.
  pure $! isNothing (commute outcome)

-- | Run a composite. Note the ST-style trick: only composites which do not
-- know anythig about the `property` type may be run.
{-# INLINE composite #-}
composite :: GlobalConfig -> (forall check . Declaration check) -> IO TestResult
composite gconf decl = do
  env <- mkEnv
  initialSeed <- Random.newSeedIO
  tvar <- initialCheckStateIO initialSeed
  outcome <- withAsynchronousChecks (asynchronousCheckThreads gconf) $ \mq ->
    try (runCompositeCheck env gconf mq tvar (runDeclaration decl id))
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
stop = Composite $ \_ _ -> throwIO (StopTestEarly "stop test" (srcLocOf callStack))

{-# INLINE stopBecause #-}
stopBecause :: HasCallStack => String -> Composite check x
stopBecause str = Composite $ \_ _ -> throwIO (StopTestEarly str (srcLocOf callStack))

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
  { maximumParallelism :: Maybe Parallelism
    -- ^ How much parallelism at most per check. If there are concurrent checks,
    -- each can use this much parallelism concurrently.
  , maximumRandomSamples :: Maybe Natural
    -- ^ Maximum number of random samples per check.
  , asynchronousCheckThreads :: Maybe Natural
    -- ^ Configure whether to allow for asynchronous checking that won't block
    -- the composite IO. This has a slight overhead: it will fork this many
    -- threads to clear a TQueue.
  }

defaultGlobalConfig :: GlobalConfig
defaultGlobalConfig = GlobalConfig
  { maximumParallelism = Nothing
  , maximumRandomSamples = Nothing
  , asynchronousCheckThreads = Nothing
  }

asynchronousChecks :: Natural -> GlobalConfig -> GlobalConfig
asynchronousChecks n gconf = gconf { asynchronousCheckThreads = Just n }

data AsyncCheckEnv where
  NoAsyncChecks :: AsyncCheckEnv
  AsyncChecks :: TSem -> TQueue QueuedCheck -> AsyncCheckEnv

-- | If there number of async check threads is configured to be a positive
-- number, then a TQueue is created and that many threads are spawned to clear
-- it and run the checks.
--
-- Those threads run forever with async exceptions masked.
-- They will block on STM retry when the queue is empty. Since the checks are
-- pure functions, they are not interruptible, but the STM retry is. So in case
-- the queue is empty, the withAsync call be able to kill the thread.
withAsynchronousChecks :: Maybe Natural
                       -> (AsyncCheckEnv -> IO t)
                       -> IO t
withAsynchronousChecks conf k = case atLeastOne of
   Nothing -> k NoAsyncChecks
   -- We know it's greater than one so we're good to go.
   Just n -> do
     tsem <- atomically (newTSem 0)
     tqueue <- newTQueueIO
     -- Call with an unmask. Seems unlikely that this would be called in an
     -- uninterruptible masking state, but I've seen enough to believe it could
     -- happen. See comment on 'clearQueue'.
     withAsyncWithUnmask (\unmask -> unmask (clearQueueN tsem tqueue n)) $ \_ ->
       -- Problem here (I think) is that we haven't guaranteed that the thread
       -- will start before the body terminates.
       -- Good solution? Make checkAsync block until it knows the threads
       -- are up.
       k (AsyncChecks tsem tqueue)
  where
    atLeastOne :: Maybe Int
    atLeastOne = do
      n <- conf
      if n > 0 then Just (fromIntegral n) else Nothing

-- | It's @IO (STM ())@ because the outer @IO@ will evaluate the test and the
-- inner will update some TMVar to communciate back to the composite main
-- thread.
newtype QueuedCheck = QueuedCheck { runQueuedCheck :: IO (STM ()) }

-- | What is given in the composite by an asynchronous check.
--
-- The STM will block until the test is done and the result is known.
newtype AsyncCheck check = AsyncCheck { asyncCheckVar :: STM Bool }

{-# INLINE awaitCheck #-}
awaitCheck :: AsyncCheck check -> Composite check Bool
awaitCheck = effect_ . atomically . asyncCheckVar

-- | Like 'check' but will do it in the background if the composite is
-- configured with async checks enabled.
--
-- Unlike typical async, if you don't await it, it (should be) is still
-- guaranteed to complete, and it will appear in the reslting check state.
--
-- To get that guarantee, this function will block until it knows that some
-- thread is going to pick up its queued check without ever having to enter into
-- an interruptible operation (i.e. an STM retry). That's done by way of an STM
-- semaphore. This call will block if there are too many other async checks
-- in progress (more than the amount of configured async check concurrency).
-- This also ensures that the backing TQueue is bounded in length.
{-# INLINE checkAsync #-}
checkAsync :: HasCallStack
           => LocalConfig
           -> RenderDomain space
           -> check specimen
           -> Domain space specimen
           -> Composite check (AsyncCheck check)
checkAsync lconf renderDomain check domain = Composite $ \mqueue k -> case mqueue of
  -- Wanted an async check, but no concurrency available. Check it here.
  NoAsyncChecks -> do
    b <- k (srcLocOf callStack) lconf renderDomain check domain
    pure $ AsyncCheck (pure b)
  -- Concurrency is available: throw the check into the queue, to fill the
  -- TMVar when it finishes.
  AsyncChecks tsem tqueue -> do
    tmvar <- newEmptyTMVarIO
    let qcheck = QueuedCheck $ do
          -- It is assumed that the check runniner `k` will evaluate the result
          -- and catch pure exceptions, so we don't need to do that here.
          b <- k (srcLocOf callStack) lconf renderDomain check domain
          pure (putTMVar tmvar b)
    -- Enqueue the check, but then wait until some worker has signalled that
    -- they're going to pick it up.
    -- See 'clearQueue' where the opposite order happens.
    atomically (writeTQueue tqueue qcheck)
    atomically (waitTSem tsem)
    pure $ AsyncCheck (readTMVar tmvar)

-- | Runs checks from the queue.
--
-- Must not be called with exceptions _uninterruptibly_ masked.
--
-- The loop will run in this interruptible mask, and should be killed by a
-- withAsync termination throwTo. It can only be interrupted when it `retry`s
-- on an empty queue looking for more work. If the queue is empty, and the
-- enclosing `withAsync` call ends, then we know the queue will forever be
-- empty, so it's time to stop.
clearQueue :: TSem -> TQueue QueuedCheck -> IO forever
clearQueue tsem tqueue = Exception.mask_ loop
  where
    loop = do
      -- Signal that we're ready and waiting for a queued check.
      atomically (signalTSem tsem)
      queuedCheck <- atomically (readTQueue tqueue)
      finish <- runQueuedCheck queuedCheck
      atomically finish
      loop

-- | Call with n > 0
clearQueueN :: TSem -> TQueue QueuedCheck -> Int -> IO ()
clearQueueN tsem tqueue n = replicateConcurrently_ n (clearQueue tsem tqueue)

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
nsamples gconf lconf = case maximumRandomSamples gconf of
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
  Just $ case maximumParallelism gconf >>= parallelismInEnv env of
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

-- | Resolved from the 'Env', 'GlobalConfig' and a 'LocalConfig'.
data ResolvedConfig = ResolvedConfig
  { resolvedRandomSamples :: !Word32
  -- ^ Always > 0
  , resolvedParallelism :: !Word32
  -- ^ Always > 0
  }

-- | The string is for an error message in case the resolution fails due to
-- clamping.
resolveConfig :: String -> Env -> GlobalConfig -> LocalConfig -> ResolvedConfig
resolveConfig errMsg env gconf lconf = ResolvedConfig
  { resolvedParallelism = m
  , resolvedRandomSamples = n
  }
  where
    !n = max 1 (clampToWord32 errMsg (nsamples gconf lconf))
    !m = case parallelism env gconf lconf of
           Nothing -> 1
           Just m' -> max 1 (clampToWord32 errMsg m')
