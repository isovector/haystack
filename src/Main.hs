module Main where

import Control.Monad.Reader (runReaderT)
import Data.Acid
import Haystack.Database
import Haystack.Game
import Haystack.User
import Haystack.Web.Routes

import Happstack.Server

makeData (a, b, c, d, e, f, g)
    = GameData { category = a
               , isFamily = b
               , isParty = c
               , isAbstract = d
               , isStrategy = e
               , is2Player = f
               , is3Player = g
               }

twister = Game "Twister"
        $ makeData ( Popular
                   , True
                   , True
                   , False
                   , False
                   , True
                   , True
                   )

bsgtbg = Game "BSGTBG"
       $ makeData ( Forgotten
                  , False
                  , False
                  , True
                  , True
                  , False
                  , True
                  )

testUser
    = User { username = "Test User"
           , owned = []
           , prefs = GamePref 4 0 0 4 4 0 0 4 0
           }

main :: IO ()
main =
    do database <- openLocalState $ Database [] []
       -- update database (AddGame twister)
       -- update database (AddGame bsgtbg)
       simpleHTTP nullConf $ do
             decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
             runReaderT routes database
