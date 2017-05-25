{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Data.Monoid
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((%~), (&), (.~), (^.))
import           Lens.Micro.TH              (makeLenses)

import           Brick.AttrMap              (attrMap)
import           Brick.BChan
import           Brick.Main                 (App (..), continue, customMain,
                                             halt, lookupExtent, showFirstCursor)
import           Brick.Types                (BrickEvent (..), EventM,
                                             Location (..), Next, Padding (Pad),
                                             Widget)
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Core         (fill, padLeft, padRight, padTop,
                                             str, reportExtent, translateBy, (<=>))

data CustomEvent = Tick deriving Show
data Direction = Dup | Ddown | Dleft | Dright
data BorderInfo = BorderInfo

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter        :: Int
       , _x                :: Int
       , _y                :: Int
       , _direction        :: Direction
       , _score            :: Int
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = znakeLayer : [baseLayer]
    where
        baseLayer = borderWithLabel (str ("Znake - " ++ show (st^.score))) (fill ' ')
        znakeLayer = translateBy (Location (st^.x, st^.y)) $ str "#"

appEvent :: St -> BrickEvent () CustomEvent -> EventM () (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent (V.EvKey V.KDown []) -> continue $ st & direction .~ Ddown
        VtyEvent (V.EvKey V.KUp []) -> continue $ st & direction .~ Dup
        VtyEvent (V.EvKey V.KLeft []) -> continue $ st & direction .~ Dleft
        VtyEvent (V.EvKey V.KRight []) -> continue $ st & direction .~ Dright
        VtyEvent _ -> continue $ st & stLastBrickEvent .~ Just e
        AppEvent Tick -> continue $ move st & stLastBrickEvent .~ Just e
        _ -> continue st
        where move st =
                case dir of
                  Ddown -> st & y %~ (+1)
                  Dup -> st & y %~ subtract 1
                  Dright -> st & x %~ (+1)
                  Dleft -> st & x %~ subtract 1
              dir = st^.direction

initialState :: St
initialState =
    St { _stLastBrickEvent = Nothing
       , _stCounter = 0
       , _x = 50
       , _y = 25
       , _direction = Dright
       , _score = 0
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
        threadDelay 250000

    void $ customMain (V.mkVty V.defaultConfig) (Just chan) theApp initialState
