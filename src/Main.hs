{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Haystack.Game
import Haystack.User

empty = makeData (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

makeData (a, b, c, d, e, f, g) = GameData { category = a
                                          , isFamily = b
                                          , isParty = c
                                          , isAbstract = d
                                          , isStrategy = e
                                          , is2Player = f
                                          , is3Player = g
                                          }

twister = Game "Twister" $ makeData ( Just Popular
                                    , Just True
                                    , Just True
                                    , Just False
                                    , Just False
                                    , Just True
                                    , Just True
                                    )

bsgtbg = Game "BSGTBG" $ makeData ( Just Forgotten
                                  , Just False
                                  , Just False
                                  , Just True
                                  , Just True
                                  , Just False
                                  , Just True
                                  )


testUser = User { username = "Test User"
                , mustBe = empty
                , prefs = GameData { category = Nothing
                                 , isFamily = Nothing
                                 , isParty = Nothing
                                 , isAbstract = Nothing
                                 , isStrategy = Nothing
                                 , is2Player = Nothing
                                 , is3Player = Just True
                                 }
                , owned = []
                }

userScore = scoreGames $ prefs testUser

main = putStrLn . show . userScore $ [twister, bsgtbg]

