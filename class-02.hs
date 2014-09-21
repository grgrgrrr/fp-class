-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms a = (div a 3600, (div a 60) - (div a 3600) * 60, a - (div a 60) * 60)   

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = h * 3600 + m * 60 + s

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]
-- True

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt (x2 - x1)^2 + sqrt (y2 - y1)^2

-- triangle :: ??? -> (Double, Double)
triangle _ = (p, s)
  where
    p = undefined
    s = undefined

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
	| mod x 2 == 0 = nEven(xs) + 1
	| otherwise = nEven(xs)

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = x*2 : doubleElems (xs)

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
	| odd x = x : fltOdd (xs)
	| otherwise = fltOdd (xs)

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
removeNeg :: Integral a => [a] -> [a]
removeNeg [] = []
removeNeg (x:xs)
	| x < 0 = removeNeg(xs)
	| otherwise = x:removeNeg(xs)

-- б) увеличить элементы с чётными значениями в два раза;
doubleEven :: Integral a => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs)
	| even x = x * 2 : doubleEven(xs)
	| otherwise = doubleEven(xs)

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).

swapOddEven :: Integral a => [a] -> [a]
swapOddEven [x] = []
swapOddEven [] = []
swapOddEven (x:y:xs) = y : x : swapOddEven(xs)

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
 
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x + y):combine_plus (xs) (ys) 

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
pairList :: [Integer] -> [Integer] -> [(Integer, Integer)]
pairList [] ys = []
pairList xs [] = []
pairList (x:xs) (y:ys) = (x, y):pairList (xs) (ys)

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
firstNUp :: Integer -> [Integer]
firstNUp 0 = []
firstNUp n = n:firstNUp (n-1) 

-- б) в порядке возрастания.
firstNDown :: Integer -> [Integer]
firstNDown n = reverse' $ firstNUp n
	where 
		reverse' [] = []
		reverse' (x:xs) = reverse' xs ++ [x]


-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insertA :: Num a => a -> [a] -> [a]
insertA a [x] = [x]
insertA a (x:xs) = x: a : insertA a xs

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
divide':: Eq a => [a] -> ([a], [a])
divide' [] = ([],[])
divide' (x:y:xs) 
	| x == y = (x:y:(fst $ divide'(xs)) , snd $ divide'(xs))
	| otherwise = (fst $ divide'(xs), x:y:(snd $ divide'(xs))) 

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a





-- б) Eq a => [a] -> a -> Bool
-- в) [a] -> Int -> [a]
-- г) a -> Int -> [a]
-- д) [a] -> [a] -> [a]
-- е) Eq a => [a] -> [[a]]
-- ж) [a] -> [(Int, a)]
-- з) Eq a => [a] -> [a]
