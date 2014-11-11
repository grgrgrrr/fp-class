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
{-toList :: Integral a => [(a, a)]  -> [a]
toList xs = foldl(\acc x -> findQ x : acc) [] xs
	where
		findQ p 
			| fst p > 0 && snd p > 0 = 1
			| fst p < 0 && snd p > 0 = 2
			| fst p < 0 && snd p < 0 = 3
			| otherwise = 4 -}

--Вариант 2
--формирует список индекса Кетле, показывающего 
--соотношение роста и веса человека для пар вида (вес, рост)
toList = foldl(\acc x -> (fst x / (snd x)^2): acc) [] 

--
toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe = undefined

toEither :: Integral a => [(a, a)]  -> Either String a
toEither = undefined

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
