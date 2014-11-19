import Control.Monad.Reader
import System.Environment
import Control.Monad
import Control.Applicative
import Data.List
import Data.Ord

data Operation = Summand | Multiplier | Division
	deriving (Show)
 
data Config = Config Operation Int
	deriving (Show)

multiplier :: Integer -> Reader Integer Integer
multiplier a = do
	n <- ask
	return $ a * n

summand :: Integer -> Reader Integer Integer
summand a = do
	n <- ask
	return $ a + n

division :: Integer -> Reader Integer Integer
division a = do
	n <- ask
	return $ div a n


loadConfig :: FilePath -> IO [Config]
loadConfig fp = readFile fp >>= (pure . ((foldl (\acc s-> makeOneConfig s : acc) []) . lines)) 

makeOneConfig :: String -> Config
makeOneConfig s 
	| n == "summand" = Config Summand (read v)
	| n == "multiplier" = Config Multiplier (read v)
	| otherwise = Config Division (read v)
	where 
		[n, e, v] = words s


	
subwork :: Reader Config ()
subwork = do
	cfg <− ask
	
	

work :: Reader Config ()
work = do
	subwork

main = getArgs >>= loadConfig >>= (\cfg −> return $ runReader work cfg)-}

