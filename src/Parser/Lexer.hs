module Lexer where
import Data.Text as T
import Data.Text (Text)
import Text.Megaparsec 
import Data.Void

type Parser = Parsec Void Text

