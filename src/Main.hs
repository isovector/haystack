{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Haystack.Game
import Haystack.User

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


testUser = User { username = "Test User"
                , prefs = GameData { category = Just Popular
                                 , isFamily = Just True
                                 , isParty = Just True
                                 , isAbstract = Nothing
                                 , isStrategy = Nothing
                                 , is2Player = Nothing
                                 , is3Player = Just True
                                 }
                , owned = []
                }

userScore = score $ prefs testUser

main = putStrLn . show . userScore $ metadata twister


