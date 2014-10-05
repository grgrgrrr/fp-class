{-
  Все задачи в этом задании должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться тремя различными тестовыми примерами, которые при запуске
  возвращают True, например:

  f = undefined -- решение 
  f_test1 = f undefined == undefined -- тест 1
  f_test2 = f undefined == undefined -- тест 2
  f_test3 = f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции об работки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти сумму и произведение элементов списка вещественных чисел.
  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход).
  d) Найти минимальный элемент списка.
  e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию).
-}
f1a:: [Integer] -> Integer
f1a = foldl (\s x -> s + if even x then x else 0) 0

f1a_test1 = f1a [1..10] == 30
f1a_test2 = f1a [] == 0
f1a_test3 = f1a [100..150] == 3250


f1b::[Float] -> (Float, Float)
f1b = foldl (\sp x -> (fst sp + x, snd sp * x)) (0, 1)

f1b_test1 = f1b [1, 2, 3, 4] == (10.0, 24.0)
f1b_test2 = f1b [1.5, 2.5, 3.43, -4.5] == (2.9300003, -57.881252)
f1b_test3 = f1b [] == (0, 1)


 
f1c::[Float] -> Float
f1c xs= (fst $ fun xs) / (snd $ fun xs)
	where  
    fun::[Float] -> (Float, Float)
    fun xs = foldl (\av x -> (fst av + x, snd av + 1)) (0, 0) xs

f1c_test1 = f1c [1, 2, 3, 4] == 2.5
f1c_test2 = f1c [1..100] == 50.5
f1c_test3 = f1c [1] == 1



f1d:: Ord a => [a] -> a
f1d xs = foldl (\min x -> if min > x then x else min) (head xs) xs

f1d_test1 = f1d [1, 2, 3, 4] == 1
f1d_test2 = f1d [3, 34, 3424, 2134, 234, 1, 23, 34, 24, 23] == 1
f1d_test3 = f1d [0] == 0



f1e::Int -> [Int]-> Int
f1e a xs 
    | (even $ head xs) && (fun == head xs)  = a
    | otherwise = fun
        where fun = foldl (\min x -> if odd x && min > x then x else min) (head xs) xs

f1e_test_1 = f1e 5 [4, 2] == 5
f1e_test2 = f1e 5 [4, 5, 6, 3, 2, 1] == 1
f1e_test2 = f1e 5 [35..100] = 35


{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий каждый второй элемент исходного.
  b) Сформировать список, содержащий первые n элементов исходного.
  c) Сформировать список, содержащий последние n элементов исходного.
  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  e) Сформировать список, содержащий все локальные минимумы исходного списка.
  f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки.
  g) Разбить список на непересекающиеся подсписки длиной n элементов.
  h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).
  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
  l) Повторить каждый элемент списка заданное количество раз.
  m) Удалить из списка повторяющиеся подряд идущие элементы.
  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать список типа [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  с) Сформировать список из первых n чисел Фибоначчи.
  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
  e) Проверить, является ли заданное целое число простым.
-}

{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 3) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}

{-
 5. Пусть числовые матрицы представлены списками строк. Реализовать следующие функции:
  1) транспонирование матрицы;
  2) сумма двух матриц;
  3) произведение двух матриц.
-}


{-
 6. Реализовать левую свёртку, пользуясь правой. Проанализировать поведение собственной реализации
  на бесконечных списках и сравнить его с поведением оригинальной foldl.
-}
