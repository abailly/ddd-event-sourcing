{-# LANGUAGE DeriveGeneric #-}
import           Control.Concurrent.STM
import           Data.Aeson
import           GHC.Generics
import           System.Environment

type Time = Double

data Point = Point (Int,Int) Time
           deriving (Read, Show, Generic)

instance ToJSON Point

pointA = Point (10,10) 0.0
pointB = Point (20,20) 12.0
pointC = Point (30,30) 22.0
pointZ = Point (40,30) 40.0

type Load = String
type Amount = Int

data Event = Moved Load Point Point
           | Ordered Route
           | Delivered Load Point Amount
           | Billed Amount
             deriving (Show, Read, Generic)

instance ToJSON Event

type Route = [Event]

data Tracker = Tracker { load  :: Load
                       , point :: Point
                       } deriving (Show, Generic)

instance ToJSON Tracker

data ShipmentStatus  = ShipmentStatus { shippings ::  [ Tracker ]
                      , currentBalance            :: Amount
                      } deriving Show

apply  :: Event -> ShipmentStatus -> ShipmentStatus
apply m@(Moved package from to) (ShipmentStatus [ (Tracker p route) ] a )
  | package == p = ShipmentStatus  [(Tracker p to)]  a


planRoute :: Load -> Point -> Point -> IO [Event]
planRoute shipment start stop = return [ Moved shipment pointA pointB
                                       , Moved shipment pointB pointC
                                       , Moved shipment pointC pointZ
                                       , Delivered shipment pointZ 3000
                                       , Billed 2600
                                       ]

commit :: Route -> IO ()
commit plannedRoute = print ("route " ++ show plannedRoute ++ " ordered")

main :: IO ()
main = do
  [ ship ] <- getArgs
  route ship

persist :: Event -> IO ()
persist event = undefined

track :: TVar ShipmentStatus -> IO ()
track system = do
  ev <- read `fmap` getLine  -- generate events
  atomically $ modifyTVar system (ev `apply`)
  system' <- atomically $ readTVar system
  persist ev
  print system'
  track system

data Command = GetStatus
             | Simulate Route
             | Commit Route
             deriving (Read)

applyCommand :: Command -> ShipmentStatus -> ShipmentStatus
applyCommand cmd shipment = undefined

handleCommands :: TVar ShipmentStatus -> IO ()
handleCommands sys = do
  cmd <- read `fmap` getLine
  sys' <- (applyCommand cmd) `fmap` atomically (readTVar sys)
  print sys'
  handleCommands sys

route :: String -> IO ()
route ship = do
  plan <- planRoute ship pointA pointZ
  -- should be something like
  -- [ move load pointA pointB (USD 1000)
  -- , move load pointB pointC (USD 1500)
  -- , move load pointC pointZ (USD 800)
  -- ]
  commit plan -- issue shipping order -> real
  sys <- newTVarIO  (ShipmentStatus  [ Tracker "toto" pointA ] 0)
  track sys
  handleCommands sys

