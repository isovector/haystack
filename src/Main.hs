module Main where

import Data.Acid
import Haystack.Database
import Haystack.Game
import Haystack.User
import Haystack.Web.Routes

import Happstack.Lite

empty = makeData (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

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
        $ makeData ( Just Popular
                   , Just True
                   , Just True
                   , Just False
                   , Just False
                   , Just True
                   , Just True
                   )

bsgtbg = Game "BSGTBG"
       $ makeData ( Just Forgotten
                  , Just False
                  , Just False
                  , Just True
                  , Just True
                  , Just False
                  , Just True
                  )

testUser
    = User { username = "Test User"
           , mustBe = empty
           , owned = []
           , prefs = GameData { category = Just Popular
                              , isFamily = Nothing
                              , isParty = Nothing
                              , isAbstract = Just False
                              , isStrategy = Nothing
                              , is2Player = Nothing
                              , is3Player = Just True
                              }}

userScore = scoreGames $ prefs testUser

nain :: IO ()
nain = do database <- openLocalState $ Database [] []
          (games, users) <- query database (GetState)
          mapM_ (putStrLn . show) games

main :: IO ()
main = serve Nothing routes
