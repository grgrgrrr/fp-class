import System.Environment
import Control.Monad.Instances

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a 
	| a `mod` 3 == 0 = 0
	| a `mod` 3 /= 0 && odd a = a^2
	| otherwise = a^3 

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте, являющемся функтором:
-}

--reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
--reduceNF n fa =  take n (iterate (fmap reduce) fa) :( 

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n fa =  foldl(\acc x -> reduce `fmap` acc) fa [1..n]

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и нетривиальным способом.
-}

--Вариант 1
--формирует список координатных чевертей для точек, заданных парами чисел
{-
toList = foldl(\acc x -> findQ x : acc) []
	where
		findQ p 
			| fst p > 0 && snd p > 0 = 1
			| fst p < 0 && snd p > 0 = 2
			| fst p < 0 && snd p < 0 = 3
			| otherwise = 4 -}

--Вариант 2
--Формирует список, содержащий стоимость товаров, если известно количество и цена  (количество, цена)
toList :: (Integral a) => [(a, a)]  -> [a]
toList = foldl (\acc x -> (fst x * snd x): acc) [] 


--Считает сумму покупки, если товаров нет на складе, возвращает Nothing
mySum xs = foldl (\acc x -> (fst x * snd x) + acc) 0 xs

toMaybe :: (Integral a) => [(a, a)]  -> Maybe a
toMaybe xs 
	| mySum xs == 0 = Nothing
	| otherwise = fmap mySum (Just xs)
	


--Считает сумму покупки, в исключительных случаях выводим сообщения 
toEither :: (Integral a) => [(a, a)]  -> Either String a
toEither xs
	| (foldl (\acc x -> fst x + acc) 0 xs) > 60 = Left("Слишком много покупок. Вы столько не унесете!")
	| mySum xs > 100000 = Left("Слишком большая сумма покупки. У вас нет столько денег!")
	| otherwise = fmap mySum (Right xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO = undefined

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs = undefined

readData :: FilePath -> IO [(Int, Int)]
readData = undefined

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  undefined
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}
