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
import Foreign.Ptr (castPtr, Ptr(..))
import Graphics.Rendering.Cairo.Internal (Render(..))
import Graphics.UI.Gtk.Gdk.EventM

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

  formulaButton <- new Gtk.ToggleToolButton [#label := "Formula"]
  #add toolBox formulaButton

  comboLabel <- new Gtk.Label [#label := "Function:"]
  #add toolBox comboLabel
  selector <- Gtk.comboBoxTextNew
  #add toolBox selector

  formula <- Gtk.labelNew Nothing
  #add toolBox formula

  fillSelector selector fs
  Gtk.comboBoxSetActive selector 0

  canvas <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  #add box canvas

  leftViewer <- Gtk.drawingAreaNew
  #add canvas leftViewer

  Gtk.widgetSetSizeRequest leftViewer 300 300

  rightViewer <- Gtk.drawingAreaNew
  #add canvas rightViewer

  Gtk.widgetSetSizeRequest rightViewer 300 300

  on leftViewer #draw $ \context -> do
    renderWithContext context $ do
      drawLeft leftViewer parallelButton trailButton formulaButton pos pos2 selector fs
    return True

  on rightViewer #draw $ \context -> do
    renderWithContext context $ do
      drawRight rightViewer parallelButton trailButton formulaButton pos pos2 selector fs
    return True

  --on leftViewer #buttonPressEvent $ touch leftViewer pos moving formulaButton pos2 moving2
  Gtk.widgetAddEvents leftViewer [EventMaskPointerMotionMask]
  Gtk.widgetAddEvents leftViewer [EventMaskButtonPressMask]
  Gtk.widgetAddEvents leftViewer [EventMaskButtonReleaseMask]
  on leftViewer #buttonPressEvent $ \e -> touch e leftViewer pos moving formulaButton pos2 moving2
  on leftViewer #motionNotifyEvent $ \e -> drag e leftViewer rightViewer pos moving pos2 moving2
  on leftViewer #buttonReleaseEvent $ \e -> released e leftViewer pos moving pos2 moving2
  on selector #changed $ updateFormula formulaButton selector fs formula
  on formulaButton #toggled $ updateFormula formulaButton selector fs formula

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

updateFormula :: Gtk.ToggleToolButton -> Gtk.ComboBoxText -> [Funcionete] -> Gtk.Label -> IO ()
updateFormula showFormula sel fs lawLabel = do
        doShow <- Gtk.toggleToolButtonGetActive showFormula
        if doShow
          then do
            selection <- Gtk.comboBoxGetActive sel
            Gtk.labelSetText lawLabel (": T(x,y) = " <> (visebleLaw (Prelude.head (Prelude.drop (fromIntegral selection) fs))))
            --Gtk.labelSetText lawLabel (": T(x,y) = ")
          else do
            Gtk.labelSetText lawLabel " "

-- touch :: Gtk.DrawingArea -> IORef [(Double, Double)] -> IORef Bool
--   -> Gtk.ToggleToolButton -> IORef (Double, Double) -> IORef Bool -> IO Bool
touch e viewer posIO movingIO turnF pos2IO moving2IO = do
  xT <- getEventButtonX e
  yT <- getEventButtonY e
  putStrLn "touch"
  putStrLn $ "drag"++ show xT ++ " " ++ show yT

  (width, height) <- Gtk.widgetGetSizeRequest viewer
  let xT' = xT - 0.5 * (fromIntegral width)
      yT' = - yT + 0.5 * (fromIntegral height)

  lisPos <- readIORef posIO
  let (x, y) = Prelude.head lisPos

  if ((sqrt ((xT' - x)^2 + (yT' - y)^2)) <= 4)
    then writeIORef movingIO True
    else return ()

  showFrame <- Gtk.toggleToolButtonGetActive turnF
  if showFrame
    then do
      (x2, y2) <- readIORef pos2IO
      if ((sqrt ((xT' - x2)^2 + (yT' - y2)^2)) <= 4)
        then writeIORef moving2IO True
        else return ()
    else return ()
  return True

-- drag     ::     Gtk.DrawingArea
--             ->     IORef [(Double, Double)]
--             ->     IORef Bool
--             ->     IORef (Double, Double)
--             ->     IORef Bool
--             ->     IO Bool
drag e leftViewer rightViewer posIO movingIO pos2IO moving2IO = do
        x <- getEventMotionX e
        y <- getEventMotionY e

        let ec = (x,y)
        (width, height) <- Gtk.widgetGetSizeRequest leftViewer
        let (xT, yT) = confineEvent ec (fromIntegral width) (fromIntegral height)
            xT' = xT - 0.5 * (fromIntegral width)
            yT' = - yT + 0.5 * (fromIntegral height)

        moving <- readIORef movingIO
        if moving
            then do lisPos <- readIORef posIO
                    writeIORef posIO ((xT', yT'):lisPos)
            else return ()

        moving2 <- readIORef moving2IO
        if moving2
            then writeIORef pos2IO (xT', yT')
            else return ()

        Gtk.widgetQueueDraw leftViewer
        Gtk.widgetQueueDraw rightViewer

        return True

-- released   ::      Gtk.DrawingArea
--            ->      IORef [(Double, Double)]
--            ->      IORef Bool
--            ->      IORef (Double, Double)
--            ->      IORef Bool
--            ->      IO Bool
released e viewer posIO movingIO pos2IO moving2IO = do
        x0 <- getEventButtonX e
        y0 <- getEventButtonY e
        let x = x0
            y = y0
            ec = (x,y)

        (width, height) <- Gtk.widgetGetSizeRequest viewer
        let (xT, yT) = confineEvent ec (fromIntegral width) (fromIntegral height)
            xT' = xT - 0.5 * (fromIntegral width)
            yT' = - yT + 0.5 * (fromIntegral height)

        moving <- readIORef movingIO
        if moving
            then do lisPos <- readIORef posIO
                    writeIORef posIO ((xT', yT'):lisPos)
                    writeIORef movingIO False
            else return ()

        moving2 <- readIORef moving2IO
        if moving2
            then do writeIORef pos2IO (xT', yT')
                    writeIORef moving2IO False
            else return ()

        Gtk.widgetQueueDraw viewer

        return True

confineEvent :: (Double, Double) -> Double -> Double -> (Double, Double)
confineEvent (x,y) width height = (xN, yN)
        where xN = if x <= 0 then 0 else if x>= width then width else x
              yN = if y <= 0 then 0 else if y>= height then height else y

parallels :: (Double, Double) -> (Double, Double) -> Bool
parallels (x1, y1) (x2, y2) =  ((delta  <= 5) && (delta  >= -5)) ||
                               ((delta2 <= 5) && (delta2 >= -5))
        where alfa1 =  truncate $ (180/pi) * atan (y1/x1)
              alfa2 =  truncate $ (180/pi) * atan (y2/x2)
              delta =  abs (alfa1 - alfa2)
              delta2 = delta - 180

turnOnElastic   :: Double
                -> Double
                -> (Double, Double)
                ->  Render ()
turnOnElastic width height (x,y) = do
    setSourceRGB 0 1 1
    let esp = 7
        alfa = if x == 0 then -pi/2 else - atan (y/x)
    if x >= 0
      then do
        arcNegative (0.5 * width)     (0.5 * height)     esp (alfa - pi/2) (alfa - 3* pi/2)
        arcNegative (0.5 * width + x) (0.5 * height - y) esp (alfa + pi/2) (alfa - pi/2)
        arcNegative (0.5 * width)     (0.5 * height)     esp (alfa - pi/2) (alfa - 3* pi/2)
        stroke
      else do
        arc (0.5 * width)     (0.5 * height)     esp (alfa - pi/2) (alfa - 3* pi/2)
        arc (0.5 * width + x) (0.5 * height - y) esp (alfa + pi/2) (alfa - pi/2)
        arc (0.5 * width)     (0.5 * height)     esp (alfa - pi/2) (alfa - 3* pi/2)
        stroke

drawRight :: Gtk.DrawingArea
          -> Gtk.ToggleToolButton
          -> Gtk.ToggleToolButton
          -> Gtk.ToggleToolButton
          -> IORef [(Double, Double)]
          -> IORef (Double, Double)
          -> Gtk.ComboBoxText
          -> [Funcionete]
          -> Render ()
drawRight rightViewer turnP turnT turnF posIO pos2IO sel fs = do

          lisPos <- liftIO $ readIORef posIO
          let pos = Prelude.head lisPos
          pos2 <- liftIO $ readIORef pos2IO
          size <- liftIO $ Gtk.widgetGetSizeRequest rightViewer
          sizeselection <- liftIO $ fromIntegral <$> Gtk.comboBoxGetActive sel
          showP <- liftIO $ Gtk.toggleToolButtonGetActive turnP
          showT <- liftIO $ Gtk.toggleToolButtonGetActive turnT
          showF <- liftIO $ Gtk.toggleToolButtonGetActive turnF
          let width = fromIntegral $ fst $ size
              height  = fromIntegral $ snd $ size
              f =  Prelude.head $ Prelude.drop sizeselection fs
              color = foregroundColor f
          clear width height (backgroundColor f)
          drawPin width height ((law f) pos) color
          if showP
           then do
                drawElastic width height ((law f) pos) color
                if parallels pos ((law f) pos) then do
                  turnOnElastic width height ((law f) pos)
                  else return ()
           else return ()
          if showF
           then do
                drawElastic width height ((law f) pos) color
                drawText width height ((law f) pos) color "Tu"
                drawPin width height ((law f) pos2) color
                drawElastic width height ((law f) pos2) color
                drawText width height ((law f) pos2) color "Tv"
                let sum = (fst pos + fst pos2, snd pos + snd pos2)
                    sumOfRange = (fst ((law f) pos) + fst ((law f) pos2),
                                      snd ((law f) pos) + snd ((law f) pos2))
                    rangeSum = ((law f) sum)

                if ( (fst sumOfRange - fst rangeSum <=2) &&
                     (snd sumOfRange - snd rangeSum <=2) )
                    then do
                      drawPin width height rangeSum color
                      drawElastic width height rangeSum color
                      drawText width height rangeSum color "Tu+Tv=T(u+v)"

                    else do
                      drawPin width height rangeSum color
                      drawElastic width height rangeSum color
                      drawText width height rangeSum color "T(u+v)"
                      drawPin width height sumOfRange color
                      drawElastic width height sumOfRange color
                      drawText width height sumOfRange color "Tu+Tv"

          else return ()

drawLeft  :: Gtk.DrawingArea
          -> Gtk.ToggleToolButton
          -> Gtk.ToggleToolButton
          -> Gtk.ToggleToolButton
          -> IORef [(Double, Double)]
          -> IORef (Double, Double)
          -> Gtk.ComboBoxText
          -> [Funcionete]
          -> Render ()
drawLeft leftViewer turnP turnT turnF posIO pos2IO sel fs = do
          lisPos <- liftIO $ readIORef posIO
          let pos = Prelude.head lisPos
          pos2 <- liftIO $ readIORef pos2IO
          size <- liftIO $ Gtk.widgetGetSizeRequest leftViewer
          sizeselection <- liftIO $ fromIntegral <$> Gtk.comboBoxGetActive sel
          showP <- liftIO $ Gtk.toggleToolButtonGetActive turnP
          showT <- liftIO $ Gtk.toggleToolButtonGetActive turnT
          showF <- liftIO $ Gtk.toggleToolButtonGetActive turnF
          let width = fromIntegral $ fst $ size
              height  = fromIntegral $ snd $ size
              f =  Prelude.head $ Prelude.drop sizeselection fs
              color = foregroundColor f
          clear width height (backgroundColor f)
          drawPin width height pos color
          if showT
             then drawTrail width height lisPos color
             else return ()
          if showP
           then do
                drawElastic width height pos color
                if parallels pos ((law f) pos) then do
                  turnOnElastic width height pos
                  else return ()
           else return ()
          if showF
           then do
                drawElastic width height pos color
                drawText width height pos color "u"
                drawPin width height pos2 color
                drawElastic width height pos2 color
                drawText width height pos2 color "v"
                let sum = (fst pos + fst pos2, snd pos + snd pos2)
                    sumOfRange = (fst ((law f) pos) + fst ((law f) pos2),
                                      snd ((law f) pos) + snd ((law f) pos2))
                    rangeSum = ((law f) sum)
                drawPin width height sum color
                drawElastic width height sum color
                drawText width height sum color "u+v"
          else return ()

clear :: Double -> Double -> (Double, Double, Double) -> Render ()
clear width height (r, g, b) = do
    setSourceRGB r g b
    paint

drawPin :: Double -> Double -> (Double, Double) -> (Double, Double, Double) -> Render ()
drawPin width height (x,y) (r, g, b) = do
    setSourceRGB r g b
    arc (0.5 * width + x) (0.5 * height - y) 3 0 (2*pi)
    fill

drawElastic     :: Double
                -> Double
                -> (Double, Double)
                -> (Double, Double, Double)
                ->  Render ()
drawElastic width height (x,y) (r, g, b) = do
    setSourceRGB r g b
    moveTo (0.5 * width) (0.5 * height)
    lineTo (0.5 * width + x) (0.5 * height - y)
    stroke

drawText     :: Double
             -> Double
             -> (Double, Double)
             -> (Double, Double, Double)
             -> String
             -> Render ()
drawText width height (x,y) (r, g, b) text = do
    setSourceRGB r g b
    textSize <- textExtents text
    let alfa = if x == 0 then pi/2 else if x > 0 then atan (y/x) else atan (y/x) + pi
        l_texto = textExtentsWidth textSize
        a_texto = textExtentsHeight textSize
    moveTo (0.5 * width + (1.1 * x) + 3 * (x/(abs x + 1)) - (l_texto/2) * (1 - cos alfa))
           (0.5 * height  - (1.1 * y) - 3 * (y/(abs y + 1)) + (a_texto/2) * (1 - sin alfa))
    showText text
    stroke


drawTrail      :: Double
                -> Double
                -> [(Double, Double)]
                -> (Double, Double, Double)
                ->  Render ()
drawTrail width height lista (r, g, b) = do
    setSourceRGB r g b
    moveTo (0.5 * width + (fst (Prelude.head lista))) (0.5 * height - (snd (Prelude.head lista)))
    continueTrail width height (Prelude.tail lista)
    stroke

continueTrail   :: Double
                -> Double
                -> [(Double, Double)]
                ->  Render ()
continueTrail width height [] = do return ()

continueTrail width height (p@(x,y):ps) = do
    lineTo (0.5 * width + x) (0.5 * height - y)
    continueTrail width height ps
    stroke
