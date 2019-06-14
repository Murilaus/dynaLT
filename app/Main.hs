{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import qualified GI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Data.GI.Base
import Data.Text
import TextShow
import Data.IORef
import GI.Cairo
import GI.Gdk
import Control.Monad.Trans.Reader (ReaderT(..))
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Internal (Render(..))

data Funcionete = Funcionete {
    name :: Text,
    pattern :: Text,
    backgroundColor :: (Double, Double, Double),
    foregroundColor :: (Double, Double, Double),
    law :: (Double, Double) -> (Double, Double),
    visebleLaw :: Text
}

main :: IO ()
main = do

  pos <- newIORef [(40.0, 40.0)]
  pos2 <- newIORef (-40.0, 40.0)

  moving <- newIORef False
  moving2 <- newIORef False

  let fs = [Funcionete "Avança 15 min"
                       "chanfrado"
                       (255/255,239/255,175/255)
                       (0/255,209/255, 222/255)
                       (\(x,y) -> (y,-x))
                       "(y,-x)",
            Funcionete "Ladeira"
                       "chanfrado"
                       (1,1,1)
                       (22/255,25/255,188/255)
                       (\(x,y) -> (x - (2*y), y - (0.5*x)))
                       "(x-2y,y-x/2)",
            Funcionete "Espelho"
                       "chanfrado"
                       (196/255,255/255,252/255)
                       (0,0,0)
                       (\(x,y) -> (-x,y))
                       "(-x,y)",
            Funcionete "Descola"
                       "chanfrado"
                       (213/255,255/255,209/255)
                       (1,0,0)
                       (\(x,y) -> (0.7 * x + 0.4 * y + 30 , 0.3 * y - 0.1 * x - 25))
                       "(0,7x+0,4y+30,0.3y-0.1x-25)"]

  Gtk.init Nothing

  win <- new Gtk.Window [ #title := "dynaLT", #defaultWidth := 675, #defaultHeight := 350 ]
  on win #destroy Gtk.mainQuit

  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #add win box

  toolBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  #add box toolBox

  parallelButton <- new Gtk.ToggleToolButton [#label := "Parallel"]
  #add toolBox parallelButton

  trailButton <- new Gtk.ToggleToolButton [#label := "Trail"]
  #add toolBox trailButton

  frameButton <- new Gtk.ToggleToolButton [#label := "Frame"]
  #add toolBox frameButton

  comboLabel <- new Gtk.Label [#label := "Function:"]
  #add toolBox comboLabel
  selector <- Gtk.comboBoxTextNew
  #add toolBox selector

  debugLabel <- Gtk.labelNew Nothing
  #add toolBox debugLabel

  fillSelector selector fs
  Gtk.comboBoxSetActive selector 0

  canvas <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  #add box canvas

  leftViewer <- Gtk.drawingAreaNew
  #add canvas leftViewer

  Gtk.widgetSetSizeRequest leftViewer 300 300

  --height <- Gtk.getWidgetHeightRequest canvas
  --width <- Gtk.widgetGetAllocatedWidth canvas

  --Gtk.labelSetText debugLabel $ showt height

  rightViewer <- Gtk.drawingAreaNew
  #add canvas rightViewer

  Gtk.widgetSetSizeRequest rightViewer 300 300

  --Gtk.widgetQueueDraw leftViewer
  -- Gtk.widgetQueueDraw rightViewer

  --renderWith leftViewer $ clear 10 10 (255/255,239/255,175/255)
  -- Gtk.onWidgetDraw leftViewer $ \ (Context fp) -> withManagedPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
  --   clear 300 300 (255/255,255/255,255/255)
  --   return True

  on leftViewer #draw $ \context -> do
    renderWithContext context $ do
      clear 300 300 (255/255,255/255,255/255)
    return True
  on rightViewer #draw $ \context -> do
    renderWithContext context $ do
      drawPin 300 300 (0,0) (0/255,0/255,0/255)
    return True

  --Gtk.widgetAddEvents leftViewer [EventMaskButtonMotionMask]
  --leftViewer `on` buttonPressEvent $ touch leftViewer pos moving frameButton pos2 moving2
  --leftViewer `on` motionNotifyEvent $ drag leftViewer pos moving pos2 moving2

  #showAll win

  Gtk.main

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))

fillSelector :: Gtk.ComboBoxText -> [Funcionete] -> IO ()
fillSelector sel (f:[]) = do
          Gtk.comboBoxTextAppend sel Nothing (name f)
          return ()
fillSelector sel (f:fs) = do
          Gtk.comboBoxTextAppend sel Nothing (name f)
          fillSelector sel fs

-- touch :: Gtk.DrawingArea -> IORef [(Double, Double)] -> IORef Bool
--  -> Gtk.ToggleToolButton -> IORef (Double, Double) -> IORef Bool -> EventM EButton Bool
-- touch viewer posIO movingIO turnF pos2IO moving2IO = do
--   xT <- getPointX
--   yT <- getPointY
--   liftIO $ do
--     (width, height) <- Gtk.widgetGetSizeRequest viewer
--     let xT' = xT - 0.5 * (fromIntegral width)
--         yT' = - yT + 0.5 * (fromIntegral height)
--
--     lisPos <- readIORef posIO
--     let (x, y) = Prelude.head lisPos
--
--     if ((sqrt ((xT' - x)^2 + (yT' - y)^2)) <= 4)
--       then writeIORef movingIO True
--       else return ()
--
--     showFrame <- Gtk.toggleToolButtonGetActive turnF
--     if showFrame
--       then do
--         (x2, y2) <- readIORef pos2IO
--         if ((sqrt ((xT' - x2)^2 + (yT' - y2)^2)) <= 4)
--           then writeIORef moving2IO True
--           else return ()
--       else return ()
--   return True

-- drag     ::     Gtk.DrawingArea
--             ->     IORef [(Double, Double)]
--             ->     IORef Bool
--             ->     IORef (Double, Double)
--             ->     IORef Bool
--             ->     EventM EMotion Bool
-- drag viewer posIO movingIO pos2IO moving2IO = do
--         x <- getPointX
--         y <- getPointY
--         let ec = (x,y)
--         liftIO $ do
--           (width, height) <- Gtk.widgetGetSizeRequest viewer
--           let (xT, yT) = confineEvent ec (fromIntegral width) (fromIntegral height)
--               xT' = xT - 0.5 * (fromIntegral width)
--               yT' = - yT + 0.5 * (fromIntegral height)
--
--           moving <- readIORef movingIO
--           if moving
--               then do lisPos <- readIORef posIO
--                       writeIORef posIO ((xT', yT'):lisPos)
--               else return ()
--
--           moving2 <- readIORef moving2IO
--           if moving2
--               then writeIORef pos2IO (xT', yT')
--               else return ()
--
--           Gtk.widgetQueueDraw viewer
--
--         return True

--confineEvent :: (Double, Double) -> Double -> Double -> (Double, Double)
confineEvent (x,y) width height = (xN, yN)
        where xN = if x <= 0 then 0 else if x>= width then width else x
              yN = if y <= 0 then 0 else if y>= height then height else y

clear :: Double -> Double -> (Double, Double, Double) -> Render ()
clear width height (r, g, b) = do
    setSourceRGB r g b
    paint

drawPin :: Double -> Double -> (Double, Double) -> (Double, Double, Double) -> Render ()
drawPin width height (x,y) (r, g, b) = do
    setSourceRGB r g b
    arc (0.5 * width + x) (0.5 * height - y) 3 0 (2*pi)
    fill
