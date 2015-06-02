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
       db <- openLocalState $ Database [] Map.empty

       case cmd of
         Just "commit" -> doCommit db
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
                do writeFile "shipping.csv" . showCSV $ export ranked
                   mapM_ putStrLn [ "everything went well!\n"
                                  , "refer to `shipping.csv` for your shipping order\n"
                                  , "run `./haystack commit` when you have sent the order"
                                  , "  so we can keep track of who has what"
                                  ]

              Left problem ->
                do hPutStrLn stderr . show $ problem
                   exitWith $ ExitFailure 1

      doCommit db =
         do csvUsers <- parseCSV <$> readFile "users.csv"
            csvGames <- parseCSV <$> readFile "games.csv"
            csvShips <- parseCSV <$> readFile "shipping.csv"
            return ()
            -- TODO: still working on this, we want it to xref
            -- between the shipping file to figure out what
            -- has been sent

      toCommit csvShips = shipCol ""
        where
            shipCol = flip (column csvShips) asString


      export ranks =
          toCSV [ "Order Number"
                , "Shipping Name"
                , "Company"
                , "Shipping Address 1"
                , "Shipping Address 2"
                , "Shipping City"
                , "Shipping State"
                , "Shipping Postal Code"
                , "Shipping Country"
                , "Email (optional)"
                , "Shipment"
                , "Contents"
                ]
                $ flip map ranks (
                    \(u, g) -> map ($ address u)
                        [ const "" -- order no
                        , shipName
                        , company
                        , ship1
                        , ship2
                        , city
                        , state
                        , postal
                        , country
                        , email
                        , const "" -- shipment
                        , const $ gameName g
                        ])
