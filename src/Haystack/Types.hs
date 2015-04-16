module Haystack.Types
    ( module Haystack.Types
    , throwError
    , lefts
    , rights
    , either
    ) where

import Control.Monad.Except hiding (msum)
import Data.Either (lefts, rights, either)
import Data.Foldable


data HaystackError = BadParse String String String
                   | MissingColumn String
                   | MissingRow String
                   | NoGames [String]
                   | OtherError String deriving (Eq)

instance Show HaystackError where
    show = ("error: " ++) . showError

type Try = Either HaystackError


colLookup :: String -> [(String, b)] -> Try b
colLookup i xs = case lookup i xs of
                   Just x  -> return x
                   Nothing -> throwError $ MissingColumn i

maybeTry :: HaystackError -> Maybe a -> Try a
maybeTry e ma = case ma of
                  Just a  -> return a
                  Nothing -> Left e

showError :: HaystackError -> String
showError (BadParse t d f) =
    "one of the csv files ("
      ++ f
      ++ ") has a bad value in one of its cells"
      ++ "\n\tlook for: "
      ++ show t
      ++ "\n\twrong because: "
      ++ d
showError (MissingColumn c) =
    "one of the csv files (not sure which) is missing column " ++ show c
showError (MissingRow c) =
    "one of the csv files (not sure which) is missing a row"
      ++ "\n\tthis usually means something exists in one but doesn't in another"
      ++ "\n\tthe column being searched is " ++ show c
showError (NoGames us) =
    msum [ "unable to match any games to the following users: "
         , msum $ map ("\n\t" ++ ) us
         , "\n\nensure games.csv has more games than users.csv has people"
         , "\n\t-- or maybe these people already own everything"
         ]
showError (OtherError s) =
    "something really bad happened"
      ++ "\n\there's all we know:"
      ++ "\n" ++ show s

