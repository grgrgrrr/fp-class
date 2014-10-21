{-
  Написать программу, которая в зависимости от параметров командной строки


-}
import System.Environment
import System.IO
import System.Random
import Data.List

type Point = (Double, Double)

-- а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
--   (по одной точке в каждой строке);
randomPointsFile :: IO()
randomPointsFile = do
	number <- randomRIO (1,100) :: IO Int
	genX <- getStdGen
	genY <- getStdGen
	writeFile "randomPointsFile.txt" (unlines $ zipWith (\a b -> show (a,b)) (newPointX number genX) (newPointY number genY))
			where 
				newPointX number genX = take number $ randomRs (-100, 100) genX :: [Int]
				newPointY number genY = take number $ randomRs (-100, 100) genY :: [Int]


-- б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
--   из четвертей;


--к какой четверти относится точка
findQ p 
	| fst p > 0 && snd p > 0 = 1
	| fst p < 0 && snd p > 0 = 2
	| fst p < 0 && snd p < 0 = 3
	| otherwise = 4  

-- в) отыскивает наиболее удалённую от начала координат точку.

main = undefined
