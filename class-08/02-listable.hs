{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}
class Listable a where
	toList:: a->[a] 
	fromList:: [a] -> a




{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integral a - любое целое число разбивается на список цифр.
-}

instance Listable String where
	toList s = words s 
