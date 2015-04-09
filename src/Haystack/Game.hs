module Haystack.Game where

import Control.Monad (liftM2)
import Data.Foldable (foldlM)

data Game = Game { name :: String
                 , metadata :: GameData
                 } deriving (Eq, Show, Read)

data Category = Popular | NewRelease | ForgottenGem deriving (Eq, Show, Read)
data GameData = GameData { category :: Maybe Category
                         , isFamily :: Maybe Bool
                         , isParty :: Maybe Bool
                         , isAbstract :: Maybe Bool
                         , isStrategy :: Maybe Bool
                         , is2Player :: Maybe Bool
                         , is3Player :: Maybe Bool
                         } deriving (Eq, Show, Read)

type GamePref = GameData

score:: GamePref -> GameData -> Maybe Int
score pref game = foldlM liftedPlus 0 subs
  where liftedPlus :: Int -> Maybe Int -> Maybe Int
        liftedPlus a (Just b) = Just $ a + b
        liftedPlus _ _ = Nothing

        check :: Eq a => (GameData -> Maybe a) -> Maybe Int
        check f = let checkP = f pref
                      checkG = f game
                   in case checkP of
                        Just p -> scoreElem $ fmap (==p) checkG
                        Nothing -> Just 0

        scoreElem :: Maybe Bool -> Maybe Int
        scoreElem (Just True)  = Just 1
        scoreElem (Just False) = Nothing
        scoreElem _            = Just 0

        subs = [ check category
               , check isFamily
               , check isParty
               , check isAbstract
               , check isStrategy
               , check is2Player
               , check is3Player
               ]

