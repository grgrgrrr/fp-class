{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
import System.Environment
import Control.Monad
import Data.List

data Student = Student {
	name :: String
	, age :: Int
	, group :: Float
	} deriving (Show)

makeTriple :: [a] -> [[a]]
makeTriple [] = []
makeTriple xs = take 3 xs : makeTriple (drop 3 xs)

main = (head `liftM` getArgs) >>= readFile >>=(print . ((foldl (\acc [n,a,g]-> Student n (read a) (read g) : acc) []) . makeTriple . lines))

