{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad.Reader (runReaderT)
import Data.Acid
import Haystack.Database
import Haystack.Game
import Haystack.User
import Haystack.Web.Routes
import System.Environment (getArgs)

import CSV
import Happstack.Server

main :: IO ()
main =
    do argv <- getArgs
       csvUsers :: CSV OfUsers <- parseCSV <$> readFile "users.csv"
       games <- csvToGames . parseCSV <$> readFile "games.csv"
       csvOwner :: CSV OfOwner <- parseCSV <$> readFile "ownership.csv"
       csvPrefs :: CSV OfPrefs <- parseCSV <$> readFile "prefs.csv"

       let user = getUser csvUsers csvPrefs csvOwner "sheehanna"
           recs = fmap (recommended games) user
           done = fmap (fmap (\x -> (gameName (fst x), snd x))) recs

       putStrLn . show $ done

       -- update database (AddGame twister)
       -- update database (AddGame bsgtbg)
       -- database <- openLocalState $ Database [] []
       {-simpleHTTP nullConf $ do-}
             {-decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)-}
             {-runReaderT routes database-}

