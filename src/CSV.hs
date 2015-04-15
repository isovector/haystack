module CSV ( CSV
           , labels
           , column
           , rows
           , rowWhere
           , parseCSV
           , columnVals
           , asBool
           , asInt
           , asString
           , OfUsers
           , OfGames
           , OfOwner) where

import Text.ParserCombinators.Parsec (sepBy, endBy, char, string, noneOf, many, parse, (<|>), (<?>), try)

import Data.List (find)
import Control.Monad (liftM)

data OfGames = OfGames
data OfUsers = OfUsers
data OfOwner = OfOwner
data CSV a = CSV { labels :: [String]
                 , rows :: [[String]]
                 } deriving (Show)

columnIndex :: CSV a -> String -> Maybe Int
columnIndex (CSV labels _) idx = lookup idx $ zip labels [0..]

column :: CSV a -> String -> (String -> b) -> [String] -> Maybe b
column csv col f row = fmap (f . (row !!)) $ columnIndex csv col

columnVals :: CSV a -> String -> (String -> b) -> Maybe [b]
columnVals csv col f = sequence . map (column csv col f) $ rows csv

rowWhere :: CSV a -> String -> (String -> Bool) -> Maybe [String]
rowWhere csv col p = find go $ rows csv
  where go = maybe False p . column csv col asString


parseCSV :: String -> CSV a
parseCSV input = CSV { labels = labels, rows = rows }
  where fileLines = case parse csvFile "(unknown)" input of
                      Left l  -> [[],[]]
                      Right r -> r
        labels = head fileLines
        rows = tail fileLines

        csvFile = endBy line eol
        line = sepBy cell (char ',')
        cell = quotedCell <|> many (noneOf ",\n\r")
        quotedCell = do char '"'
                        content <- many quotedChar
                        char '"' <?> "quote at end of cell"
                        return content
        quotedChar =
            noneOf "\""
            <|> try (string "\"\"" >> return '"')
        eol =
            try (string "\n\r")
            <|> try (string "\r\n")
            <|> string "\n"
            <|> string "\r"
            <?> "end of line"

asBool :: String -> Bool
asBool = ("X" ==)

asInt :: String -> Int
asInt = read

asString :: String -> String
asString = id

