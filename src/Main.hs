module Main where

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
           , prefs = GamePref 5 1 1 5 5 1 1 5 1
           }

nain :: IO ()
nain = simpleHTTP nullConf $
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
       routes

main = mapM_ (putStrLn . show) $ recommended [twister, bsgtbg] testUser
