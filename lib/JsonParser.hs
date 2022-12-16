module JsonParser where
import Text.Parsec
import Datastructures
import Text.Parsec.String (Parser)
import qualified Datastructures as Actions
-- help for parsec: https://jakewheat.github.io/intro_to_parsing/

--basics
--make level parser that parses the whole level = loose pairs

parseLevel :: Parser JSON
parseLevel = Object <$> (whitespace *> many1 parsePair <* whitespace)

parseString :: Parser JSON
parseString =
  String <$> (char '"' *> many1 (letter <|> space) <* char '"')

parseNumber :: Parser JSON
parseNumber = Number . read <$> many1 digit

parseJSON :: Parser JSON
parseJSON = whitespace *> parseNumber <|> parseString <|> parseArray <|> parseObject <|> parseActions <|> parseUsetimes <|> parseLayout <|> parseLevel <* whitespace

parseArray :: Parser JSON
parseArray = Array <$> (char '[' *> whitespace *> sepBy parseJSON (char ',') <* whitespace <* char ']')

parseObject :: Parser JSON
parseObject = Object <$> ( whitespace *> char '{' *> whitespace *> sepBy parsePair (char ',') <* whitespace <*char '}' <* whitespace)

parseObjID :: Parser ID
parseObjID = ID <$> (whitespace *> many1 letter <* whitespace)

parsePair :: Parser Pair
parsePair = whitespace *> parseObjID >>= \name -> if name== (ID "layout") then Pair name <$> (whitespace*> char ':' *> parseLayout) else Pair name <$> (char ':' *> whitespace *> parseJSON <* whitespace)

--functions
parseFunction :: Parser Function
parseFunction = parseObjID >>= \name -> Function name <$> (char '(' *> sepBy parseArgument (char ',') <* char ')')

parseArgument :: Parser Argument
parseArgument = parseArgumentFunction <|> parseTargetID

parseArgumentFunction :: Parser Argument
parseArgumentFunction = ArgumentFunction <$> parseFunction

parseTargetID :: Parser Argument
parseTargetID = TargetID <$> parseObjID

parseAction :: Parser Action
parseAction = (char '[' *> sepBy parseFunction (char ',') <* char ']') >>= \conditions -> Action conditions <$> parseFunction

parseActions :: Parser JSON
parseActions = Actions <$> parseAction

--usetimes
parseUsetimes :: Parser JSON
parseUsetimes = parseAmount <|> parseUnlimited

parseAmount :: Parser JSON
parseAmount = UseTimes <$> (Amount . read <$> many1 digit)

parseUnlimited :: Parser JSON
parseUnlimited = UseTimes <$> (string "infinite" *> return Infinite)

--layout
parseLayout :: Parser JSON
parseLayout = Layout <$> (whitespace *> char '{' *> whitespace *> char '|' *> whitespace *> sepBy parseTileLine (char '|') <* whitespace <* char '}' <* whitespace)

parseTileLine :: Parser TileLine
parseTileLine = TileLine <$> (whitespace *> sepBy parseTile (char ' ') <* whitespace)

parseTile :: Parser Tile
parseTile =parseWall <|> parseFloor <|> parseStart <|> parseEnd

parseWall :: Parser Tile
parseWall = char '*' *> return Wall

parseFloor :: Parser Tile
parseFloor = char '.' *> return Floor

parseStart :: Parser Tile
parseStart = char 's' *> return Start

parseEnd :: Parser Tile
parseEnd = char 'e' *> return End

whitespace :: Parser ()
whitespace = skipMany (oneOf " \t\n")
