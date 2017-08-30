{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Data.Monoid
import qualified Graphics.Vty               as V
import           Lens.Micro                 (ix, mapped, (%~), (&), (.~), (^.))
import           Lens.Micro.TH              (makeLenses)
import           System.Random              (getStdGen, randomRs)

import           Brick.AttrMap              (attrMap)
import           Brick.BChan
import           Brick.Main                 (App (..), continue, customMain,
                                             halt, lookupExtent, lookupViewport,
                                             showFirstCursor)
import           Brick.Types                (BrickEvent (..), EventM,
                                             Location (..), Next, Padding (Pad),
                                             Widget, locationColumnL,
                                             locationRowL)
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Core         (fill, hBox, hLimit, padLeft,
                                             padRight, padTop, reportExtent,
                                             str, translateBy, vBox, vLimit,
                                             (<=>))

data CustomEvent = Tick deriving Show
data Direction = Dup | Ddown | Dleft | Dright deriving (Eq)
data Status = Running | GameOver | Pause
data BorderInfo = BorderInfo

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter        :: Int
       , _zHead            :: Location
       , _zTail            :: [Location]
       , _direction        :: Direction
       , _food             :: Location
       , _nextFoods        :: [Location]
       , _score            :: Int
       , _status           :: Status
       , _height           :: Int
       , _width            :: Int
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [center $ hLimit (st^.width) $ vLimit (st^.height) $ borderWithLabel (str (uiTitle (st^.status) (st^.score))) $ vBox rows]
  where
    rows = [hBox $ cellsInRow r | r <- [0..st^.height - 1]]
    cellsInRow y = [drawLocation (Location (x, y)) | x <- [0..st^.width-1]]
    drawLocation l
      | l `elem` st^.zTail = str "Z"
      | l == st^.zHead     = str "Z"
      | l == st^.food      = str "O"
      | otherwise          = str " "

uiTitle :: Status -> Int -> String
uiTitle Running s  = "Znake - " ++ show s
uiTitle Pause s    = "Paused - " ++ show s
uiTitle GameOver s = "Game Over - " ++ show s

appEvent :: St -> BrickEvent () CustomEvent -> EventM () (Next St)
appEvent st (VtyEvent (V.EvKey V.KEsc []))        = halt st
appEvent st (VtyEvent (V.EvKey V.KDown []))       = continue $ turn Ddown st
appEvent st (VtyEvent (V.EvKey V.KUp []))         = continue $ turn Dup st
appEvent st (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn Dleft st
appEvent st (VtyEvent (V.EvKey V.KRight []))      = continue $ turn Dright st
appEvent st (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause st
appEvent st (AppEvent Tick)                       = continue $ tick st
appEvent st _                                     = continue st

--Gameplay Helpers
pause :: St -> St
pause st =
  case st^.status of
    Running -> st & status .~ Pause
    Pause   -> st & status .~ Running
    _       -> st

tick :: St -> St
tick = eatFood . checkWallCollision . checkZnakeCollision . move

move :: St -> St
move st =
  case st^.status of
    Running  -> case curDir st of
      Ddown  -> st & zHead.locationRowL %~ (+1) & posTail
      Dup    -> st & zHead.locationRowL %~ subtract 1 & posTail
      Dright -> st & zHead.locationColumnL %~ (+1) & posTail
      Dleft  -> st & zHead.locationColumnL %~ subtract 1 & posTail
    Pause    -> st
    GameOver -> st
  where posTail = zTail .~ (st^.zHead) : init (st^.zTail)

curDir :: St -> Direction
curDir st = st^.direction

grow :: St -> St
grow st = st & zTail %~ ((st^.zHead):)

turn :: Direction -> St -> St
turn d st = st & direction .~ newDir
  where newDir =
          case d of
            Ddown  -> if curDir st == Dup then Dup else Ddown
            Dup    -> if curDir st == Ddown then Ddown else Dup
            Dleft  -> if curDir st == Dright then Dright else Dleft
            Dright -> if curDir st == Dleft then Dleft else Dright

checkZnakeCollision :: St -> St
checkZnakeCollision st = if st^.zHead `elem` st^.zTail then st & status .~ GameOver else st

checkWallCollision :: St -> St
checkWallCollision st = if outHorizontal || outVertical then st & status .~ GameOver else st
  where outHorizontal = (st^.zHead.locationColumnL) < 0 || (st^.zHead.locationColumnL) > (st^.width - 3)
        outVertical   = (st^.zHead.locationRowL) < 0 || (st^.zHead.locationRowL) > (st^.height - 3)

eatFood :: St -> St
eatFood st = if st^.zHead == st^.food then scoreAndGrow st else st
  where incScore st = st & score %~ (+1)
        scoreAndGrow = getNextFood . incScore . grow

getNextFood :: St -> St
getNextFood st = st & food .~ newFood & nextFoods .~ next
  where newFood:next = st^.nextFoods
--End Gameplay Helpers

foodLocations :: St -> IO [Location]
foodLocations st = do
  g <- getStdGen
  let x = randomRs (1, st^.width - 3) g
  let y = randomRs (1, st^.height - 3) g
  return $ Location <$> zip x y

startGame :: IO St
startGame = do
  let st = initialState
  fs <- foodLocations st
  return $ st & nextFoods .~ fs

initialState :: St
initialState =
    St { _stLastBrickEvent = Nothing
       , _stCounter        = 0
       , _zHead            = Location (10, 10)
       , _zTail            = [Location (49,15), Location (48, 15)]
       , _direction        = Dright
       , _food             = Location (5,5)
       , _nextFoods        = []
       , _score            = 0
       , _status           = Running
       , _height           = 20
       , _width            = 60
       }

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main :: IO ()
main = do
    chan <- newBChan 10

    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100000

    st <- startGame

    void $ customMain (V.mkVty V.defaultConfig) (Just chan) theApp st
