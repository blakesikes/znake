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
                                             halt, showFirstCursor)
import           Brick.Types                (BrickEvent (..), EventM, Next,
                                             Widget)
import           Brick.Types                (Padding (Pad))
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Core         (fill, padLeft, padRight, padTop,
                                             str, (<=>))

data CustomEvent = Counter deriving Show

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter        :: Int
       , _x                :: Int
       , _y                :: Int
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = znakeLayer : [baseLayer]
    where
        baseLayer = borderWithLabel (str "Znake") (fill ' ')
        znakeLayer =  padLeft (Pad (st^.x)) $ padTop (Pad (st^.y)) (str "#")

appEvent :: St -> BrickEvent () CustomEvent -> EventM () (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent _ -> continue $ st & stLastBrickEvent .~ (Just e)
        AppEvent Counter -> continue $ st & stCounter %~ (+1)
                                          & stLastBrickEvent .~ (Just e)
        _ -> continue st

initialState :: St
initialState =
    St { _stLastBrickEvent = Nothing
       , _stCounter = 0
       , _x = 50
       , _y = 25
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
        writeBChan chan Counter
        threadDelay 1000000

    void $ customMain (V.mkVty V.defaultConfig) (Just chan) theApp initialState
