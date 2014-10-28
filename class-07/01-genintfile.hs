{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}
import System.Environment
import System.IO
import System.Random
import Data.List

createFile  :: String -> Int -> Int -> Int -> Int -> IO ()
createFile fname from to countNum countS = do
	genP <- getStdGen
	writeFile fname (allNewDigits genP)
			where 
				newDigits genP = take countNum $ randomRs (from, to) (mkStdGen genP) :: [Int]

				oneString genP = unwords $ map (show) $ newDigits genP

				allNewDigits genP = unlines $ tail $ map(fst) $ take (countS+1) (iterate (\x -> (oneString $ snd x, snd x + 10)) ("", 1))



main = do
  [fname, from, to, countNum, countString] <- getArgs
  createFile fname (read from) (read to) (read countNum) (read countString)
