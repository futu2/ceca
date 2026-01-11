module Ceca.Parser where

import Ceca.AST
import Ceca.Parser.Basic
import Ceca.Parser.Expr
import Ceca.Parser.Pattern
import Ceca.Parser.Type
import Text.Megaparsec

-- Main entry points
parseProgram :: Parser Expr
parseProgram = spaceConsumer *> parseExpr <* eof

parseTypeOnly :: Parser Type
parseTypeOnly = spaceConsumer *> parseType <* eof

parsePatternOnly :: Parser Pattern
parsePatternOnly = spaceConsumer *> parsePattern <* eof
