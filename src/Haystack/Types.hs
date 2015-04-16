module Haystack.Types
    ( module Haystack.Types
    , throwError
    , lefts
    , rights
    , either
    ) where

import Control.Monad.Except
import Data.Either (lefts, rights, either)
import Data.Foldable


data HaystackError = ParseError String
                   | MissingColumn String
                   | MissingRow String
                   | OtherError String deriving (Eq, Show)

type Try = Either HaystackError


colLookup :: String -> [(String, b)] -> Try b
colLookup i xs = case lookup i xs of
                   Just x  -> return x
                   Nothing -> throwError $ MissingColumn i

maybeTry :: HaystackError -> Maybe a -> Try a
maybeTry e ma = case ma of
                  Just a  -> return a
                  Nothing -> Left e
