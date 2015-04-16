{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Haystack.Database where

import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.State  (get, put)
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Happstack.Server (ServerPartT)

type Owned = (String, [String])


data Database = Database [Owned] [Owned] deriving (Typeable)



$(deriveSafeCopy 0 'base ''Database) -- '

type App = ReaderT (AcidState Database) (ServerPartT IO)

clearStage :: Update Database ()
clearStage = do Database owns _ <- get
                put $ Database owns []

stageOwner :: String -> String -> Update Database ()
stageOwner u g = do Database owns staged <- get
                    put $ Database owns ((u, [g]):staged)

merge :: Eq a => [(a, [b])] -> [(a, [b])] -> [(a, [b])]
merge xs ys = merge' xs ys []
  where
      merge' _ [] ws = ws
      merge' [] ys _ = ys
      merge' xs ((k, y):ys) ws =
          (case lookup k xs of
            Just x  -> (k, x ++ y)
            Nothing -> (k, y)) : merge' xs ys ws

commitStage :: Update Database ()
commitStage = do Database owns staged <- get
                 put $ Database (merge owns staged) []

getOwned :: Query Database [Owned]
getOwned = do Database owns _ <- ask
              return owns

getStage :: Query Database [Owned]
getStage = do Database _ staged <- ask
              return staged

$(makeAcidic ''Database [ 'clearStage
                        , 'stageOwner
                        , 'commitStage
                        , 'getStage
                        , 'getOwned])

