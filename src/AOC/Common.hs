module AOC.Common where

import qualified Text.Parsec as P


type Parser = P.Parsec String ()

parser :: Parser a -> String -> Either P.ParseError a
parser p = P.parse p ""