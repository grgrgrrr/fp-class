{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Environment
import System.IO


countLines :: String -> IO()
countLines fname = do
	contents <- readFile fname	
	print $ count contents
		where count contents = foldl (\acc x -> if x == '\n' then acc + 1 else acc) 0 contents


addFirst :: String -> String -> IO()
addFirst fname s = do
	contents <- readFile fname
	writeFile "tmp.txt" s
	writeFile "tmp.txt" contents
	


main = do
	[fname, s] <- getArgs
	countLines fname
	addFirst fname s
	


