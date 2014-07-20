-- | Provides functionality of fixed frame rate management.
module FixedFrameRateManager
    ( FrameRateManager
    , DelayedFrameProcessing (..)
    , FrameAction (..)
    , checkNextAction
    , doUpdating
    , doDrawing
    , runFrameRateManager
    , getFps
    ) where


import Control.Monad.State


-- | Calculates the next step and the passed frames from the drawn step.
calcNextStep
    :: Int -- ^ the value of fps
    -> Double -- ^ the drawn step
    -> Double -- ^ current time
    -> (Double, Int) -- ^ the next step and the passed frames
calcNextStep fps drawnStep currentTime =
    loop drawnStep 0
  where
    spf = 1.0 / (realToFrac fps)

    loop step frames =
        if step > currentTime
            then (step, frames)
            else loop (step + spf) (frames + 1)


-- | Specifies processes for the delayed frames.
data DelayedFrameProcessing =
      Drop -- ^ the delayed frames are updated but not drawn
    | Slowdown -- ^ the delayed frames are updated and drawn


-- | Specifies required action for the client of frame rate management.
data FrameAction =
      None -- ^ action is not required
    | Updating -- ^ required action is updating
    | Drawing -- ^ required action is drawing


-- | Represents status of frame rate management.
data FrameInfo = FrameInfo
    { fps :: Int
    , dfp :: DelayedFrameProcessing
    , drawnStep :: Double
    , updatedFrames :: Int
    , nextStep :: Double
    }


-- | Represents instructions which are managed by frame rate management.
type FrameRateManager = StateT FrameInfo


-- | Gets the value of fps.
getFps :: Monad m
    => FrameRateManager m Int -- ^ the value of fps
getFps = do
    FrameInfo fps _ _ _ _ <- get
    return fps


-- | Checks next action for the client of frame rate management.
checkNextAction :: Monad m
    => Double -- ^ current time
    -> FrameRateManager m FrameAction -- ^ next action
checkNextAction currentTime = do
    FrameInfo fps dfp drawnStep updatedFrames _ <- get

    -- Calculates the next step and the passed frames from the drawn step.
    let (nextStep, frames) = calcNextStep fps drawnStep currentTime

    case (frames <= 0) of

        -- If the passed frame is zero, nothing is done because current time is before the drawn step.
        True  -> return None

        -- If the passed frames are more than 1, does updating or drawing.
        False -> do
            case dfp of
                Drop -> do
                    case (updatedFrames < frames) of

                        -- If the updated frames are less than the passed frames, does updating because updating has not caught up.
                        True  -> return Updating

                        -- If the updated frames equal the passed frames, does drawing because updating has caught up.
                        False -> do
                            -- Updates nextStep to set after drawing.
                            put $ FrameInfo fps dfp drawnStep updatedFrames nextStep
                            return Drawing
                Slowdown -> do
                    case (updatedFrames == 0) of

                        -- If the frame has not been updated, does updating.
                        True  -> return Updating

                        -- If the frame has been updated, does drawing.
                        False -> do
                            -- Updates nextStep to set after drawing.
                            put $ FrameInfo fps dfp drawnStep updatedFrames nextStep
                            return Drawing


-- | Performs updating action and transits the new state of frame rate management.
doUpdating :: Monad m
    => m a -- ^ updating action
    -> FrameRateManager m a -- ^ the result of updating action
doUpdating action = do
    ret <- lift action
    FrameInfo fps dfp drawnStep updatedFrames nextStep <- get

    -- Increments a count of the updated frames.
    put $ FrameInfo fps dfp drawnStep (updatedFrames + 1) nextStep

    return ret


-- | Performs drawing action and transits the new state of frame rate management.
doDrawing :: Monad m
    => m a -- ^ drawing action
    -> FrameRateManager m a -- ^ the result of drawing action
doDrawing action = do
    ret <- lift action
    FrameInfo fps dfp _ _ nextStep <- get

    -- Resets a count of the updated frames and sets nextStep to drawnStep.
    put $ FrameInfo fps dfp nextStep 0 nextStep

    return ret


-- | Runs frame rate management.
runFrameRateManager :: Monad m
    => Int -- ^ the value of fps
    -> DelayedFrameProcessing -- ^ a process for the delayed frames
    -> FrameRateManager m a -- ^ instructions which are managed by frame rate management
    -> m a -- ^ the result of instructions
runFrameRateManager fps dfp s = evalStateT s (FrameInfo fps dfp 0 0 0)
