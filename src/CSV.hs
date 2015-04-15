module CSV (CSV
           , column
           , rows
           , rowWhere
           , parseCSV
           , columnVals
           , asBool
           , asInt
           , asString
           , OfUsers
           , OfGames) where

import Data.List (find)
import Data.List.Split (splitOn)
import Control.Monad (liftM)

data OfGames = OfGames
data OfUsers = OfUsers
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
parseCSV file = CSV { labels = labels, rows = rows }
  where fileLines = map (splitOn ",") $ lines file
        labels = head fileLines
        rows = tail fileLines

asBool :: String -> Bool
asBool = ("x" ==)

asInt :: String -> Int
asInt = read

asString :: String -> String
asString s = case s of
               "\"\"" -> ""
               x -> x

