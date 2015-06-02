module CSV where

import Text.ParserCombinators.Parsec (sepBy, endBy, char, string, noneOf, many, parse, (<|>), (<?>), try)

import Data.List (find)
import Control.Monad (liftM)
import Safe

import Haystack.Types

data OfGames = OfGames
data OfUsers = OfUsers
data OfOwner = OfOwner
data OfPrefs = OfPrefs
data OfShips = OfShips
data CSV a = CSV { labels :: [String]
                 , rows :: [[String]]
                 }

instance Show (CSV a) where
    show = showCSV

showLineCSV :: [String] -> String
showLineCSV = init . ((++ ",") . show =<<)

columnIndex :: CSV a -> String -> Try Int
columnIndex (CSV labels _) idx = colLookup idx $ zip labels [0..]

column :: CSV a -> String -> (String -> Try b) -> [String] -> Try b
column csv col f row = (f . (row !!)) =<< columnIndex csv col

columnVals :: CSV a -> String -> (String -> Try b) -> Try [b]
columnVals csv col f = sequence . map (column csv col f) $ rows csv

rowWhere :: CSV a -> String -> (String -> Bool) -> Try [String]
rowWhere csv col p = maybeTry (MissingRow $ "comparing column " ++ col) . find go $ rows csv
  where go = either (const False) p . column csv col asString

showCSV :: CSV a -> String
showCSV (CSV labels rows) = unlines $ return (showLineCSV labels) ++ map showLineCSV rows

toCSV :: [String] -> [[String]] -> CSV a
toCSV labels rows = CSV { labels = labels
                        , rows = rows
                        }

parseCSV :: String -> CSV a
parseCSV input = CSV { labels = labels, rows = filter ((== length labels) . length) rows }
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

asBool :: String -> Try Bool
asBool mb = case mb of
             "X" -> return True
             ""  -> return False
             _   -> throwError $ BadParse mb
                                          "use X to indicate `yes`, leave blank for `no`"
                                          "games.csv, probably"

asInt :: String -> Try Int
asInt mi = case readMay mi of
             Just i  -> return i
             Nothing -> throwError $ BadParse mi
                                              "not a number"
                                              "prefs.csv, probably"

asString :: String -> Try String
asString = return . id

