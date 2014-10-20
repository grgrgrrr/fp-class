{-
  Разработайте утилиту со следующими возможностями:

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Environment
import System.IO
import System.Directory
import Data.Char
import System.Random

-- 1) подсчёт количества строк в заданном текстовом файле;
countLines :: String -> IO()
countLines fname = do
	contents <- readFile fname	
	print $ count contents
		where count contents = foldl (\acc x -> if x == '\n' then acc + 1 else acc) 0 contents


-- 2) добавление заданной строки в начало (конец) заданного файла;
addFirst :: String -> String -> IO()
addFirst fname s = do
	contents <- readFile fname
	writeFile "tmp.txt" (s ++ "\n"  ++ contents)
	removeFile fname
	renameFile "tmp.txt" fname


addLast :: String -> String -> IO()
addLast fname s = do
	contents <- readFile fname
	writeFile "tmp.txt" (contents ++ "\n"  ++ s)
	removeFile fname
	renameFile "tmp.txt" fname	


--  3) преобразование всех буквенных символов заданного файла к верхнему
--     регистру (результат выводится на консоль)
toUpperFile :: String -> IO()
toUpperFile fname = do
	contents <- readFile fname
	putStrLn (map toUpper contents)

--  4) построчное слияние двух заданных файлов (каждая строка первого файла
--     соединяется с соответствующей строкой второго файла);
mergeFiles :: String -> String -> IO()
mergeFiles fname1 fname2 = do
	contents1 <- readFile fname1
	contents2 <- readFile fname2
	let linesOfFile1 = lines contents1
	let linesOfFile2 = lines contents2
	writeFile "merged.txt" (unlines $ zipWith (\a b ->  a ++ "\n" ++ b) linesOfFile1 linesOfFile2)

-- 5) генерация случайного текстового файла (случайность должна ограничиваться
--    максимальным количеством строк в файле и символов в строке).
randomFile :: IO()
randomFile = do
	number <- randomRIO (1,100) :: IO Int
	numberLen <- randomRIO (1,100) :: IO Int
	gen <- getStdGen
	writeFile "randomFile.txt" (concat $ replicate number ((take numberLen $ randomRs ('a','z') gen) ++ "\n"))



main = do
	[fname, fname2, s] <- getArgs
	countLines fname
	addFirst fname s
	addLast fname s
	toUpperFile fname
	mergeFiles fname fname2
	randomFile


