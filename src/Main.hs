{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (mapM_, forM_, concat, elem)
import Data.Foldable
import Data.List (elemIndex, (\\))
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.Reader (runReaderT)
import System.Environment (getArgs)
import System.IO
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

rankUsers :: CSV OfUsers
          -> CSV OfPrefs
          -> CSV OfOwner
          -> [Owned]
          -> [Game]
          -> Try [(User, Game)]
rankUsers csvUsers csvPrefs csvOwner owned games =
    do usernames <- columnVals csvUsers "username" asString
       users <- sequence $ map (getUser csvUsers csvPrefs csvOwner owned) usernames
       let rankings = into . zip users $ map (recommended games) users
           ranked   = sortBy (flip $ comparing (snd . snd)) rankings
           allocated = allocateGames games ranked

       if length allocated == length users
          then return allocated
          else throwError . NoGames . map username $ (users \\ (map fst allocated))
  where into x = concat $ map (\(a, bs) -> map ((,) a) bs) x


main :: IO ()
main =
    do argv <- getArgs
       db <- openLocalState $ Database [] []

       if elem "commit" argv
          then update db CommitStage
          else doRanking db

  where
      doRanking db =
         do csvUsers <- parseCSV <$> readFile "users.csv"
            csvOwner <- parseCSV <$> readFile "ownership.csv"
            csvPrefs <- parseCSV <$> readFile "prefs.csv"
            games <- csvToGames . parseCSV <$> readFile "games.csv"

            owned <- query db GetOwned
            case rankUsers csvUsers csvPrefs csvOwner owned games of
              Right ranked ->
                do update db ClearStage
                   mapM_ (withRanking db) ranked

              Left problem -> hPutStrLn stderr . show $ problem


      withRanking db (u, g) =
          do let gn = gameName g
             putStrLn (show u ++ (show gn))
             update db (StageOwner (username u) gn)

       -- update database (AddGame twister)
       -- update database (AddGame bsgtbg)
       {-simpleHTTP nullConf $ do-}
             {-decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)-}
             {-runReaderT routes database-}

