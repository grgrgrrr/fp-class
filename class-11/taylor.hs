import Control.Monad.Writer

eps = 0.0000000001



taylorSin :: Double -> Double -> Double -> Double -> Writer [Double] Double
taylorSin x sum prevN i 
	| abs (prevN - nextN) < eps = tell [prevN] >> return sum
	| otherwise = tell[prevN] >> taylorSin x (sum+prevN) nextN (i+1)
	where 
		nextN = (prevN * (-1.0) * x * x) / (2*i) / (2*i + 1)

taylorCos :: Double -> Double -> Double -> Double -> Writer [Double] Double
taylorCos x sum prevN i 
	| abs (prevN - nextN) < eps = tell [prevN] >> return sum
	| otherwise = tell[prevN] >> taylorCos x (sum+prevN) nextN (i+1)
	where 
		nextN = (prevN * (-1.0) * x * x) / (2*i) / (2*i - 1)

sinJournal :: Double -> (Double, [Double])
sinJournal x = runWriter $ taylorSin x x x 1

cosJournal :: Double -> (Double, [Double])
cosJournal x = runWriter $ taylorCos x x x 1
