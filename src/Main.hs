{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (mapM_, forM_, concat, elem)
import qualified Data.Map as Map
import Data.Foldable
import Data.List (elemIndex, (\\))
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.Reader (runReaderT)
import Safe (headMay)
import System.Environment (getArgs)
import System.Exit
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

serverConfig :: Conf
serverConfig = nullConf { port      = 8080
                        }

main :: IO ()
main =
    do cmd <- headMay <$> getArgs
       db <- openLocalState $ Database [] [] Map.empty

       case cmd of
         Just "commit" -> do update db CommitStage
                             putStrLn "updating user ownership with last ranking"
         Just "help"   -> doHelp
         Just "server" -> doServer  db
         Just "rank"   -> doRanking db
         Nothing       -> doRanking db

  where
      doHelp =
          do mapM_ putStrLn [ "usage: ./haystack [cmd]\n"
                            , "Haystack is a game ranking platform for Gamebox Monthly.\n"
                            , "optional arguments:"
                            , "   help\t\tshow this help"
                            , "   commit\tconfirm that the last order sheet was sent"
                            , "   server\trun the web application"
                            , "   rank\t\toutput an order sheet to `order.csv`"
                            ]


      doServer db =
          do putStrLn "running haystack web application on port 80"
             simpleHTTP serverConfig $ do
               decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
               runReaderT routes db

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

              Left problem ->
                do hPutStrLn stderr . show $ problem
                   exitWith $ ExitFailure 1


      withRanking db (u, g) =
          do let gn = gameName g
             putStrLn (show u ++ (show gn))
             update db (StageOwner (username u) gn)

