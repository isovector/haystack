{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (mapM_, forM_, concat)
import Data.Foldable
import Data.List (elemIndex)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.Reader (runReaderT)
import System.Environment (getArgs)
import Utils (showTrace, unwrapPair)

import CSV
import Haystack
import Haystack.Web.Routes


type Inventory = (Game, Int)
type UserRank = (User, (Game, Int))

updateWith :: (Eq a, Eq b) => [(a, b)] -> (b -> Maybe b) -> a -> [(a, b)]
updateWith [] e k = []
updateWith ((k', v):kvs) e k
    | k' == k   = case e v of
                    Just v' -> (k, v'):kvs
                    Nothing -> kvs
    | otherwise = (k', v) : updateWith kvs e k

allocateGames :: [Game] -> [UserRank] -> [(User, Game)]
allocateGames games rank = forceState . runState (allocateGamesImpl rank) $ buildQuotas games
  where buildQuotas = ap zip (map inventory) . filter ((> 0) . inventory)
        forceState = uncurry (flip seq)

allocateGamesImpl :: [UserRank] -> State [Inventory] [(User, Game)]
allocateGamesImpl [] = return []
allocateGamesImpl ((user, (game, _)):ranks) =
    do inventory' <- get
       if isJust $ lookup game inventory'
          then do put $ updateWith inventory' (\i -> if i <= 1
                                                    then Nothing
                                                    else Just $ i - 1) game
                  next <- allocateGamesImpl $ filter ((/= user) . fst) ranks
                  return $ (user, game) : next
          else allocateGamesImpl ranks

rankUsers :: CSV OfUsers -> CSV OfPrefs -> CSV OfOwner -> [Game] -> Try [(User, Game)]
rankUsers csvUsers csvPrefs csvOwner games =
    do usernames <- columnVals csvUsers "username" asString
       users <- sequence $ map (getUser csvUsers csvPrefs csvOwner) usernames
       let rankings = into . zip users $ map (recommended games) users
           ranked   = sortBy (flip $ comparing (snd . snd)) rankings
       return $ allocateGames games ranked
  where into x = concat $ map (\(a, bs) -> map ((,) a) bs) x


main :: IO ()
main =
    do argv <- getArgs
       csvUsers <- parseCSV <$> readFile "users.csv"
       csvOwner <- parseCSV <$> readFile "ownership.csv"
       csvPrefs <- parseCSV <$> readFile "prefs.csv"
       games <- csvToGames . parseCSV <$> readFile "games.csv"

       let rankings = rankUsers csvUsers csvPrefs csvOwner games

       case rankings of
         Right ranked ->
           mapM_ (\(u, g) -> putStrLn . show $ (username u, gameName g)) ranked
         Left problem -> putStrLn . show $ problem

       {-let user = getUser csvUsers csvPrefs csvOwner "sheehanna"-}
           {-recs = fmap (recommended games) user-}
           {-done = fmap (fmap (\x -> (gameName (fst x), snd x))) recs-}

       -- update database (AddGame twister)
       -- update database (AddGame bsgtbg)
       -- database <- openLocalState $ Database [] []
       {-simpleHTTP nullConf $ do-}
             {-decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)-}
             {-runReaderT routes database-}

