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
    Database [Owned] [Owned] (Map (String, String) GamePref) deriving (Typeable)


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

setPrefs :: (String, String) -> GamePref -> Update Database ()
setPrefs u p = do Database owns staged prefs <- get
                  put . Database owns staged
                      $ Map.insert u p prefs

commitStage :: Update Database ()
commitStage = do Database owns staged prefs <- get
                 put $ Database (merge (++) owns staged) [] prefs

getOwned :: Query Database [Owned]
getOwned = do Database owns _ _ <- ask
              return owns

getStage :: Query Database [Owned]
getStage = do Database _ staged _ <- ask
              return staged

getPrefs :: [(String, String)] -> Query Database [((String, String), GamePref)]
getPrefs reqs =
    do Database _ _ prefs <- ask
       return . filter (flip elem reqs . fst) $ Map.assocs prefs

$(makeAcidic ''Database [ 'clearStage
                        , 'stageOwner
                        , 'commitStage
                        , 'getStage
                        , 'getOwned
                        , 'getPrefs
                        , 'setPrefs])

