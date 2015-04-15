{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (mapM_, forM_)
import Data.Foldable (mapM_, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Control.Monad.Reader (runReaderT)
import Data.Acid
import Data.Maybe (fromMaybe)
import Haystack.Database
import Haystack.Game
import Haystack.User
import Haystack.Web.Routes
import System.Environment (getArgs)

import CSV
import Happstack.Server

--rankUsers :: CSV OfUsers -> CSV OfPrefs -> CSV OfOwner -> [Game] -> Maybe [(User, (Game, Int))]
rankUsers csvUsers csvPrefs csvOwner games =
    do usernames <- columnVals csvUsers "username" asString
       users <- sequence $ map (getUser csvUsers csvPrefs csvOwner) usernames
       return . zip users $ map (head . recommended games) users


main :: IO ()
main =
    do argv <- getArgs
       csvUsers <- parseCSV <$> readFile "users.csv"
       csvOwner <- parseCSV <$> readFile "ownership.csv"
       csvPrefs <- parseCSV <$> readFile "prefs.csv"
       games <- csvToGames . parseCSV <$> readFile "games.csv"

       let rankings = rankUsers csvUsers csvPrefs csvOwner games

       mapM_ (
           mapM_ (
               \(u, (g, i)) -> putStrLn . show $ (username u, gameName g))) rankings

       {-let user = getUser csvUsers csvPrefs csvOwner "sheehanna"-}
           {-recs = fmap (recommended games) user-}
           {-done = fmap (fmap (\x -> (gameName (fst x), snd x))) recs-}

       -- update database (AddGame twister)
       -- update database (AddGame bsgtbg)
       -- database <- openLocalState $ Database [] []
       {-simpleHTTP nullConf $ do-}
             {-decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)-}
             {-runReaderT routes database-}

