-- | Provides functionality of fixed step management.
module FixedStepManager
    ( StepManagerT
    , StepAction (..)
    , checkNextAction
    , doUpdate
    , doDrawing
    , runStepManager
    ) where


import Control.Monad.State


-- | Specifies required action for the client of step management.
data StepAction =
      None -- ^ action is not required
    | Update -- ^ required action is update
    | Drawing -- ^ required action is drawing


-- | Represents status of step management.
data StepInfo f = StepInfo f f Bool


-- | Represents instructions which are managed by step management.
type StepManagerT f = StateT (StepInfo f)


-- | Next action calculated from current time.
nextAction :: (Fractional f, Ord f)
    => StepInfo f
    -> f    -- ^ current time
    -> StepAction
nextAction (StepInfo _ nextUpdateTime updated) currentTime
  | currentTime >= nextUpdateTime = Update
  | updated                       = Drawing
  | otherwise                     = None


-- | Checks next action for the client of step management.
checkNextAction :: (Fractional f, Ord f, Monad m)
    => f    -- ^ current time
    -> StepManagerT f m StepAction  -- ^ next action
checkNextAction currentTime = do
    s <- get
    return $ nextAction s currentTime


-- | Performs update action and transits the new state of step management.
doUpdate :: (Fractional f, Ord f, Monad m)
    => m a -- ^ update action
    -> StepManagerT f m a -- ^ the result of update action
doUpdate action = do
    ret <- lift action
    StepInfo step nextUpdateTime _ <- get

    -- Increments the next update time and sets the updated flag to true.
    put $ StepInfo step (nextUpdateTime + step) True

    return ret


-- | Performs drawing action and transits the new state of step management.
doDrawing :: (Fractional f, Ord f, Monad m)
    => m a -- ^ drawing action
    -> StepManagerT f m a -- ^ the result of drawing action
doDrawing action = do
    ret <- lift action
    StepInfo step nextUpdateTime _ <- get

    -- Resets the updated flag to false.
    put $ StepInfo step nextUpdateTime False

    return ret


-- | Runs step management.
runStepManager :: (Fractional f, Ord f, Monad m)
    => f -- ^ the value of step period
    -> StepManagerT f m a -- ^ instructions which are managed by step management
    -> m a -- ^ the result of instructions
runStepManager step s = evalStateT s (StepInfo step 0 False)
