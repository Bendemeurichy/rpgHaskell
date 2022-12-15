import Text.Parsec
import Datastructures
-- help for parsec: https://jakewheat.github.io/intro_to_parsing/





parseString :: Parser JSON
parseString =
  String <$> (char '"' *> many1 (letter <|> space) <* char '"')

parseNumber :: Parser JSON
parseNumber = Number . read <$> many1 digit

parseJson :: Parser JSON
parseJson = parseNumber <|> parseString <|> parseArray <|> parseObject <|> parseFunction

parseArray :: Parser JSON
parseArray = Array <$> (char '[' *> sepBy parseJSON (char ',') <* char ']')

parseFunction :: Parser JSON
parseFunction = parseObjID >>= \name -> Function name <$> (char '(' *> parseJSON <* char ')'))

parseObject :: Parser JSON
parseObject = Object <$> (char '{' *> sepBy parsePair (char ',') <* char '}')

parseObjID :: Parser ID
parseObjID = ID <$> (char '"' *> many1 letter <* char '"')

parsePair :: Parser Pair
parsePair = parseObjID >>= \name -> Pair name <$> (char ':' *> parseJSON)