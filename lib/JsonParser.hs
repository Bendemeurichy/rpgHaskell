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
parseNumber = Number . read <$> many1 digit <* whitespace

parseJSON :: Parser JSON
parseJSON =parseNumber <|> parseString <|> parseArray <|> parseObject <|> parseActions <|> parseUsetimes <|> parseLayout <|> parseLevel

parseArray :: Parser JSON
parseArray = Array <$> (char '[' *> whitespace *> sepBy parseJSON (char ',') <* whitespace <* char ']')

parseObject :: Parser JSON
parseObject = Object <$> ( whitespace *> char '{' *> whitespace *> sepBy parsePair (char ',') <* whitespace <*char '}' <* whitespace)

parseObjID :: Parser ID
parseObjID = ID <$> (whitespace *> many1 letter <* whitespace)

parsePair :: Parser Pair
parsePair = do
  name <-whitespace >> parseObjID
  if name == ID "layout" then Pair name <$> (whitespace >> char ':' >> parseLayout)
    else if name == ID "actions"
      then Pair name <$> (whitespace >> char ':' >> parseActions)
    else Pair name <$> (char ':' >> whitespace >> parseJSON <* whitespace)

--functions

parseFunction :: Parser Function
parseFunction = whitespace *> parseObjID >>= \name -> Function name <$> (char '(' *> sepBy parseArgument (char ',') <* char ')' <*whitespace)

parseArgument :: Parser Argument
parseArgument = try parseArgumentFunction <|> parseTargetID

parseArgumentFunction :: Parser Argument
parseArgumentFunction = ArgumentFunction <$> parseFunction

parseTargetID :: Parser Argument
parseTargetID = TargetID . ID <$> (whitespace *> many alphaNum <* whitespace)

parseAction :: Parser Action
parseAction = (whitespace *> char '[' *> sepBy parseFunction (char ',') <* char ']' <* whitespace) >>= \conditions -> Action conditions <$> (whitespace *>parseFunction <* whitespace)

parseActions :: Parser JSON
parseActions = Actions <$> (whitespace *> char '{' *> whitespace *> sepBy parseAction (char ',') <* whitespace <* char '}' <* whitespace)

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
