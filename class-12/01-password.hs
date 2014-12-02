{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import System.Environment
import Data.Char

data Config = Config Int Bool Bool Bool
	deriving (Show)

isValid :: String -> Config -> Bool
isValid s (Config mlen isl isd isp) = (length s >= mlen) && 
                (if isl then any isAlpha s else True) && 
                (if isd then any isNumber s else True) && 
                (if isp then any isPunctuation s else True)

getValidPassword :: MaybeT (WriterT [String] (ReaderT Config IO)) String
getValidPassword = do
  config <- lift $ lift ask
  lift $ lift $ lift $ putStrLn "Введите новый пароль:" 
  s <- lift $ lift $ lift getLine
  lift $ tell [s]
  guard (isValid s config)
  return s
 
askPassword :: MaybeT (WriterT [String] (ReaderT Config IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  lift $ lift $ lift $ putStrLn "Сохранение в базе данных..."

parseConfig :: [String] -> Config
parseConfig (mlen : isl : isd : isp : _) = Config (read mlen) (read isl) (read isd) (read isp)

main = do
	config <- getArgs
	k <-  runReaderT (runWriterT  (runMaybeT askPassword)) (parseConfig config)
        print $ snd k
