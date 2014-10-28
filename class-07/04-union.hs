{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import qualified Data.Set as Set

readNumFile :: FilePath -> IO [Int]
readNumFile fname = do
	content <- readFile fname
  	let xs = map read $ concatMap words $ lines content
	return xs

solve :: [[Int]] -> (Int, [Int])
solve xs = (Set.size mySet, (Set.foldl (+) 0 mySet))
	where 
		mySet = Set.unions $ map Set.fromList xs

main = getArgs >>= mapM readNumFile >>= print.solve