{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (sortBy, elemIndex)
import Data.Ord (comparing)
import Prelude hiding (mapM_, forM_)
import Data.Foldable (mapM_, forM_)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Control.Monad (ap)
import Control.Monad.Reader (runReaderT)
import Data.Acid
import Data.Maybe (fromMaybe)
import Haystack.Database
import Haystack.Game
import Haystack.User
import Haystack.Web.Routes
import System.Environment (getArgs)
import Utils (showTrace, unwrapPair)

import CSV
import Happstack.Server


type Inventory = (Game, Int)
type UserRank = (User, (Game, Int))

buildQuotas :: [Game] -> [Inventory]
buildQuotas = ap zip (map inventory) . filter ((> 0) . inventory)

into :: [(a, [b])] -> [(a, b)]
into x = concat $ map (\(a, bs) -> map ((,) a) bs) x

updateWith :: (Eq a, Eq b, Show a, Show b) => [(a, b)] -> (b -> Maybe b) -> a -> [(a, b)]
updateWith set update key =
    case showTrace $ lookup key set of
      Just b -> replace set
                        (showTrace $ unwrapPair (key, update b))
                        (fromMaybe 0 $ elemIndex (key, b) set)
      Nothing -> set
  where replace :: [a] -> Maybe a -> Int -> [a]
        replace [] _ _ = []
        replace (_:xs) (Just val) 0 = val:xs
        replace (_:xs) Nothing 0 = xs
        replace (x:xs) val i = x : (replace xs val $ i - 1)

allocateGames :: [Game] -> [UserRank] -> [(User, Game)]
allocateGames games rank = let written = runWriterT $ allocateGamesImpl rank
                               ((_, log), _) = runState written $ buildQuotas games
                            in log

allocateGamesImpl :: [UserRank] -> WriterT [(User, Game)] (State [Inventory]) ()
allocateGamesImpl [] = return ()
allocateGamesImpl ((user, (game, _)):ranks) =
    do tell [(user, game)]
       inventory' <- get
       return $ updateWith inventory' (\i -> if i <= 0
                                          then Nothing
                                          else Just $ i - 1) game
       allocateGamesImpl $ filter ((/= user) . fst) ranks

rankUsers :: CSV OfUsers -> CSV OfPrefs -> CSV OfOwner -> [Game] -> Maybe [(User, Game)]
rankUsers csvUsers csvPrefs csvOwner games =
    do usernames <- columnVals csvUsers "username" asString
       users <- sequence $ map (getUser csvUsers csvPrefs csvOwner) usernames
       let rankings = into . zip users $ map (recommended games) users
           ranked   = sortBy (flip $ comparing (snd . snd)) rankings
       return $ allocateGames games ranked


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
               \(u, g) -> putStrLn . show $ (username u, gameName g))) rankings

       {-let user = getUser csvUsers csvPrefs csvOwner "sheehanna"-}
           {-recs = fmap (recommended games) user-}
           {-done = fmap (fmap (\x -> (gameName (fst x), snd x))) recs-}

       -- update database (AddGame twister)
       -- update database (AddGame bsgtbg)
       -- database <- openLocalState $ Database [] []
       {-simpleHTTP nullConf $ do-}
             {-decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)-}
             {-runReaderT routes database-}

