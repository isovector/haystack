{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Haystack.Database where

import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.State  (get, put)
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Happstack.Server (ServerPartT)

import Haystack.Game

type Owned = (String, [String])


data Database =
    Database [Owned] [Owned] [(String, GamePref)] deriving (Typeable)


$(deriveSafeCopy 0 'base ''Database) -- '
$(deriveSafeCopy 0 'base ''GamePref) -- '

type App = ReaderT (AcidState Database) (ServerPartT IO)


merge :: Eq a => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
merge f xs ys = merge' xs ys []
  where
      merge' _ [] ws = ws
      merge' [] ys _ = ys
      merge' xs ((k, y):ys) ws =
          (case lookup k xs of
            Just x  -> (k, f x y)
            Nothing -> (k, y)) : merge' xs ys ws


clearStage :: Update Database ()
clearStage = do Database owns _ prefs <- get
                put $ Database owns [] prefs

stageOwner :: String -> String -> Update Database ()
stageOwner u g = do Database owns staged prefs <- get
                    put $ Database owns ((u, [g]):staged) prefs

setPrefs :: String -> GamePref -> Update Database ()
setPrefs u p = do Database owns staged prefs <- get
                  put . Database owns staged
                      . merge (flip const) prefs
                      $ return (u, p)

commitStage :: Update Database ()
commitStage = do Database owns staged prefs <- get
                 put $ Database (merge (++) owns staged) [] prefs

getOwned :: Query Database [Owned]
getOwned = do Database owns _ _ <- ask
              return owns

getStage :: Query Database [Owned]
getStage = do Database _ staged _ <- ask
              return staged

$(makeAcidic ''Database [ 'clearStage
                        , 'stageOwner
                        , 'commitStage
                        , 'getStage
                        , 'getOwned
                        , 'setPrefs])

