import Control.Monad.State

type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = do
	xs <- get
	put (xs++[x]) 

dequeue :: State Queue Int
dequeue = do
	(x:xs) <- get
	put xs
	return x

queueManip :: State Queue Int
queueManip = do
	enqueue 3
	enqueue 5
	enqueue 7
	a <- dequeue
	dequeue

j = runState queueManip
