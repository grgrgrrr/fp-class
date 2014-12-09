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
float = fl <|> fromIntegral <$> natural 
	where 
	fl = do
		n <- natural
		char '.'
		m <- natural
		return $ fromIntegral n + (fromIntegral m / (10 ^ countNum m)) 

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = do
	char '('
	n <- float
	char ','
	m <- float
	char ')'
	return $ (n, m) 

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = do
	char '['
	k <- sepBy complex (symbol ";") 
	char ']'
	return $ k

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = do
	char '['
	k <- sepBy (complexOrFloat) (symbol ";") 
	char ']'
	return $ k

complexOrFloat :: Parser (Float, Float)
complexOrFloat = complex <|> fl
	where 
	fl = do
	num <- float
	return $ (num, 0)

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" (sepBy (complexOrFloat) (symbol ","))


