import System.Environment
import Control.Monad.Instances

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength xs = foldl1 (+) $ fmap (\x -> length x) xs

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 ch 0 = Nothing
build1 ch n = Just ([replicate c ch | c <- [1..n]])

{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 ch n
	| n == 0 = Left ("n = 0")
	| n>100 = Left ("n > 100")
	| ch == 'x' = Left ("Роспотребнадзор запрещает создавать строки из символа x")
	| otherwise = Right ([replicate c ch | c <- [1..n]])

{-
  Параметрами командной строки являются имя файла, символ, целое число.
-}

{- 1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки. -}
{-  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).-}
totalLinesLength :: String -> IO Int
totalLinesLength fname = (totalLength . lines) `fmap` readFile fname

{- 3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов). -}
totalLengthAll1 ch num = totalLength `fmap` (build1 (read ch) (read num))

totalLengthAll2 ch num = totalLength `fmap` (build2 (read ch) (read num))


main = do
	[fname, ch, num] <- getArgs

	k<- totalLength `fmap` getArgs
	print k

    	i<- totalLinesLength fname
    	print i

	print $ totalLengthAll1 ch num

	print $ totalLengthAll2 ch num



	

