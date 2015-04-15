{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad.Reader (runReaderT)
import Data.Acid
import Haystack.Database
import Haystack.Game
import Haystack.User
import Haystack.Web.Routes

import CSV
import Happstack.Server

main :: IO ()
main =
    do database <- openLocalState $ Database [] []
       csvUsers :: CSV OfUsers <- parseCSV <$> readFile "users.csv"
       csvGames :: CSV OfGames <- parseCSV <$> readFile "games.csv"
       csvOwner :: CSV OfOwner <- parseCSV <$> readFile "ownership.csv"
       mapM_ (putStrLn . show) . csvToGames $ csvGames
       putStrLn . show $ getUser csvUsers "isovector"
       putStrLn . show $ getOwnership csvOwner "annametreveli"

       -- update database (AddGame twister)
       -- update database (AddGame bsgtbg)
       {-simpleHTTP nullConf $ do-}
             {-decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)-}
             {-runReaderT routes database-}

