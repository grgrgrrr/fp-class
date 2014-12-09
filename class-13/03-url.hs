import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show
type Server = String
type Login = String 
type Password = String
type Host = String
type Port =  String
type Path = String
type Params = String
type Anchor = String

data URL =  URL Scheme Login Password Server Port Path Params Anchor 
	deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

login :: Parser Login
login = many1 (sat (/=':'))

password :: Parser Password
password = char ':' >> many1 (sat (/= '@'))

host :: Parser Host
host = char '@' >> many1 (sat(/= ':'))

port :: Parser Port
port = char ':' >> many1 (sat(/= '/'))

path :: Parser Path
path = char '/' >> many1 (sat(/= '?'))

params :: Parser Params
params = char '?' >> many1 (sat(/= '#'))

anchor :: Parser Anchor
anchor = char '#' >> many (sat $ const True)




url = URL <$>
      scheme <*>
      (string "://" >> login) <*> 
	password <*>
	host <*>
	port <*>
	params <*>
	anchor 


