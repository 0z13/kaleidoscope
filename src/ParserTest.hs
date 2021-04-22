{-# LANGUAGE RecordWildCards   #-}
module ParserTest where
import Control.Applicative hiding (some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Lexer


uri :: Parser Scheme 
uri = choice [  
  HttpsScheme <$ string "https",
  HttpScheme <$ string "http",
  FishScheme <$ string "fish" ]


data Authority = Authority
  { authUser :: Text  -- (user, password)
  , authPass :: Text
  } deriving (Eq, Show)


data Scheme 
  = HttpScheme
  | HttpsScheme
  | FishScheme
  deriving (Show, Eq)

data Uri = Uri
  { uriScheme :: Scheme,
    uriAuthority :: Authority 
  } deriving (Eq, Show)

pAuthority :: Parser Authority 
pAuthority =  do
  void (string "//")
  user <- T.pack <$> some alphaNumChar 
  void (char ':')
  pass <- T.pack <$> some alphaNumChar
  void (char '@')
  pure Authority { authUser=user, authPass=pass} 


pUri :: Parser Uri
pUri = do
  r <- uri 
  s <- pAuthority 
  pure Uri {uriScheme=r, uriAuthority=s}

