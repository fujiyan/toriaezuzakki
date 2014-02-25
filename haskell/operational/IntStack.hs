{-# LANGUAGE GADTs #-}


import Control.Monad.Operational


type Stack = [Int]

data StackI a where
    Push :: Int -> StackI ()
    Pop :: StackI Int

type StackP a = Program StackI a


push :: Int -> StackP ()
push = singleton . Push

pop :: StackP Int
pop = singleton Pop

interpret :: StackP a -> Stack -> Stack
interpret = eval . view
  where
    eval :: ProgramView StackI a -> Stack -> Stack
    eval (Push x :>>= is) stack     = interpret (is ()) (x:stack)
    eval (Pop    :>>= is) (x:stack) = interpret (is x) stack
    eval (Return _) stack           = stack


add :: StackP ()
add = do
    x <- pop
    y <- pop
    push (x + y)


main :: IO ()
main = (putStrLn . show) $ interpret add [1, 2]
