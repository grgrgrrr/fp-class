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
import Control.Applicative
import Data.List
import Data.Ord

data Student = Student {
	name :: String
	, age :: Int
	, group :: Float
	} deriving (Eq)

instance Ord Student where
  compare (Student name1 age1 gr1) (Student name2 age2 gr2) = compare name1 name2

instance Show Student where
	show (Student name1 age1 gr1) = show name1 ++ "\n" ++ show age1 ++ "\n" ++ show gr1 + "\n"


makeTriple :: [a] -> [[a]]
makeTriple [] = []
makeTriple xs = take 3 xs : makeTriple (drop 3 xs)

makeList :: String -> IO [Student]
makeList fp = readFile fp >>= (pure . ((foldl (\acc [n,a,g]-> Student n (read a) (read g) : acc) []) . makeTriple . lines))

makeMerge :: String -> String ->  IO ()
makeMerge name1 name2 = (++) `liftM` makeList name1 `ap` makeList name2 >>= (writeToFile . sort)

writeToFile :: [Student] -> IO ()
writeToFile s =  return (show s) >>= writeFile "allStudents.txt"

main = makeMerge "students1.txt" "students2.txt"

