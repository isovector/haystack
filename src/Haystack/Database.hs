{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Haystack.Database where

import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.State  (get, put)
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Haystack.Game
import Haystack.User
import Happstack.Server (ServerPartT)


data Database = Database [Game] [User] deriving (Typeable)

$(deriveSafeCopy 0 'base ''Category) -- '
$(deriveSafeCopy 0 'base ''Database) -- '
$(deriveSafeCopy 0 'base ''Game)     -- '
$(deriveSafeCopy 0 'base ''GameData) -- '
$(deriveSafeCopy 0 'base ''GamePref) -- '
$(deriveSafeCopy 0 'base ''User)     -- '
$(deriveSafeCopy 0 'base ''ShipAddr) -- '

type App = ReaderT (AcidState Database) (ServerPartT IO)

addGame :: Game -> Update Database ()
addGame game = do Database games users <- get
                  put $ Database (game:games) users

getState :: Query Database ([Game], [User])
getState = do Database games users <- ask
              return (games, users)

$(makeAcidic ''Database ['addGame, 'getState])

