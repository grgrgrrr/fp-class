import Control.Monad
import Control.Monad.Reader
import System.Environment

data Operation = Summand | Multiplier | Division
	deriving (Show)
 
data Config = Config Operation Int
	deriving (Show)

data Configs = Configs [Config]


loadConfig :: FilePath -> IO Configs
loadConfig fp = readFile fp >>= (return . Configs . ((foldl (\acc s-> makeOneConfig s : acc) []) . lines)) 

makeOneConfig :: String -> Config
makeOneConfig s 
	| n == "summand" = Config Summand (read v)
	| n == "multiplier" = Config Multiplier (read v)
	| otherwise = Config Division (read v)
	where 
		[n, e, v] = words s

op :: Config -> Int -> Int
op (Config Summand i) = (+ i)
op (Config Multiplier i) = (* i)
op (Config Division i) = (`div` i)


work :: Int -> Reader Configs String
work num = do
	Configs a <- ask
	return $ show $ foldl (flip op) num a
 

toInts :: String -> [Int]
toInts s = map read $ concat $ map words $ lines s

main = do
	content <- readFile "digits.txt"
	conf <- loadConfig "configs.txt"
  	let c = map (\x -> runReader  (work x) conf) (toInts content)
	mapM print c
