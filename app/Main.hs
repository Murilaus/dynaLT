{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Data.GI.Base
import Data.Text
import Data.IORef

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

  fillSelector selector fs
  Gtk.comboBoxSetActive selector 0

  leftVisor <- Gtk.drawingAreaNew
  #add box leftVisor

  rightVisor <- Gtk.drawingAreaNew
  #add box rightVisor

  Gtk.widgetQueueDraw leftVisor
  Gtk.widgetQueueDraw rightVisor

  --renderWith leftVisor $ clear 10 10 (255/255,239/255,175/255)
  --Gtk.onWidgetDraw leftVisor $ clear 10 10 (255/255,239/255,175/255)

  #showAll win

  Gtk.main

fillSelector :: Gtk.ComboBoxText -> [Funcionete] -> IO ()
fillSelector sel (f:[]) = do
          Gtk.comboBoxTextAppend sel Nothing (name f)
          return ()
fillSelector sel (f:fs) = do
          Gtk.comboBoxTextAppend sel Nothing (name f)
          fillSelector sel fs

clear :: Double -> Double -> (Double, Double, Double) -> Render ()
clear largura altura (r, g, b) = do
    setSourceRGB r g b
    paint
    --Create a circle in the center
    setSourceRGB 0.7 0.7 0.7
    arc (0.5 * largura) (0.5 * altura) 3 0 (2*pi)
    stroke
