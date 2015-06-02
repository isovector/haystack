{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Haystack.Database where

import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.State  (get, put)
import Data.Acid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Typeable
import Happstack.Server (ServerPartT)

import Haystack.Game
import Utils (showTrace)

type Owned = (String, [String])


data Database =
    Database [Owned] (Map (String, String) GamePref) deriving (Typeable)


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

setPrefs :: (String, String) -> GamePref -> Update Database ()
setPrefs u p = do Database owns prefs <- get
                  put . Database owns
                      $ Map.insert u p prefs

commitStage :: Update Database ()
commitStage = do Database owns prefs <- get
                 put $ Database owns prefs

getOwned :: Query Database [Owned]
getOwned = do Database owns _ <- ask
              return owns

getPrefs :: [(String, String)] -> Query Database [((String, String), GamePref)]
getPrefs reqs =
    do Database _ prefs <- ask
       return . filter (flip elem reqs . fst) $ Map.assocs prefs

$(makeAcidic ''Database [ 'commitStage
                        , 'getOwned
                        , 'getPrefs
                        , 'setPrefs])

