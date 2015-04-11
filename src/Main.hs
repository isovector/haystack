module Main where

import Data.Acid
import Haystack.Database
import Haystack.Game
import Haystack.User
import Haystack.Web.Routes

import Happstack.Server

empty = makeData (Popular, False, False, False, False, False, False)

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
           , mustBe = empty
           , owned = []
           , prefs = GameData { category = Popular
                              , isFamily = False
                              , isParty = False
                              , isAbstract = False
                              , isStrategy = False
                              , is2Player = False
                              , is3Player = True
                              }}

main :: IO ()
main = simpleHTTP nullConf $
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
       routes
