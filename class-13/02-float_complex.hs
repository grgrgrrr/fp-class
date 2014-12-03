import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative
import Control.Monad

{- Напишите парсер для вещественных чисел. -}

countNum :: Int -> Int
countNum 0 = 0
countNum num = countNum (div num 10) + 1   

--парсер для Float
float :: Parser Float
float = do
	n <- natural
	char '.'
	m <- natural
	return $ n + m / (10 * countNum n) 

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = undefined

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = undefined

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = undefined

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = undefined


