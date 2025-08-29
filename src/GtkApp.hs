module GtkApp (mainGtkApp) where

import CairoDraw (calculateCanvasSize, drawGrid)
import Codec.Picture (readImage)
import Codec.Picture.Types (DynamicImage)
import Control.Lens (makeLenses)
import Control.Monad (void)
import Control.Monad.Except
import Data.Either (fromRight)
import Data.IORef
import Data.Maybe
import Debug.Trace
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import ImageProcessing (colourGrid)
import InputForm hiding (validateForm)
import Linear (V0 (V0))
import StitchConfig (StitchConfig)
import Text.Read (readMaybe)

data Form = MkForm
  { _stitchGauge :: IORef Double,
    _rowGauge :: IORef Double,
    _numberOfColours :: IORef Int,
    _targetStitches :: IORef Int,
    _imageFilePath :: IORef String
  }

mainGtkApp :: IO ()
mainGtkApp = do
  -- Initialize GTK+
  void initGUI

  form <- initialForm

  -- Create the main window
  window <- windowNew
  set
    window
    [ windowTitle := "GTK+ App with Input and Drawing",
      windowDefaultWidth := 600,
      windowDefaultHeight := 400
    ]

  -- Connect the destroy event to quit the GTK+ main loop
  _ <- on window objectDestroy mainQuit

  -- Create a vertical box to arrange widgets
  vbox <- vBoxNew False 5 -- False for non-homogeneous, 5 for spacing

  -- --- File Chooser Section ---
  fileChooserButton <- buttonNewWithLabel "Select File"
  _ <- on fileChooserButton buttonActivated $ do
    -- Create a file chooser dialog
    dialog <-
      fileChooserDialogNew
        (Just "Choose an image")
        (Just window) -- Parent window
        FileChooserActionOpen
        [ ("Open", ResponseAccept),
          ("Cancel", ResponseCancel)
        ]

    -- Run the dialog and get the response
    response <- dialogRun dialog

    case response of
      ResponseAccept -> do
        -- User clicked "Open", get the selected filename
        mFilename <- fileChooserGetFilename dialog
        case mFilename of
          Just filename -> writeIORef (_imageFilePath form) filename
          Nothing -> putStrLn "No file selected."
      _ -> do
        -- User clicked "Cancel" or closed the dialog
        putStrLn "File selection cancelled."

    -- Destroy the dialog when done
    widgetDestroy dialog

  -- Add the file chooser button to the vertical box
  boxPackStart vbox fileChooserButton PackNatural 0

  -- --- Text Entry Section ---
  (stitchGauge, stitchGaugeBox) <- inputText "Stitch gauge stitch/cm" "2.1"
  (rowGauge, rowGaugeBox) <- inputText "Row gauge row/cm" "1.9"
  (numberOfColours, numberOfColoursBox) <- inputText "Number of colours" "3"
  (targetStitches, targetStitchesBox) <- inputText "Target stitches" "100"

  -- Horizontal box for size input
  hboxSize <- hBoxNew False 5

  -- Horizontal box for X position input
  hboxXPos <- hBoxNew False 5

  -- Add input boxes to the vertical box
  boxPackStart vbox hboxSize PackNatural 0
  boxPackStart vbox hboxXPos PackNatural 0
  boxPackStart vbox stitchGaugeBox PackNatural 0
  boxPackStart vbox rowGaugeBox PackNatural 0
  boxPackStart vbox numberOfColoursBox PackNatural 0
  boxPackStart vbox targetStitchesBox PackNatural 0

  -- --- Drawing Area Section ---
  drawingArea <- drawingAreaNew

  scrolledWindow <- scrolledWindowNew Nothing Nothing
  set
    scrolledWindow
    [ scrolledWindowHscrollbarPolicy := PolicyAlways,
      scrolledWindowVscrollbarPolicy := PolicyAlways
    ]
  containerAdd scrolledWindow drawingArea

  -- Draw Button
  drawButton <- buttonNewWithLabel "Create stitch diagram"
  _ <- on drawButton buttonActivated $ do
    stitchGauge <- entryGetText stitchGauge
    case readMaybe stitchGauge :: Maybe Double of
      Just sg | sg > 0 -> writeIORef (_stitchGauge form) sg
      _ -> putStrLn $ "Invalid stitch gauge input: " ++ stitchGauge

    rowGauge <- entryGetText rowGauge
    case readMaybe rowGauge :: Maybe Double of
      Just rg | rg > 0 -> writeIORef (_rowGauge form) rg
      _ -> putStrLn $ "Invalid row gauge input: " ++ rowGauge

    numberOfColoursText <- entryGetText numberOfColours
    case readMaybe numberOfColoursText :: Maybe Int of
      Just nc | nc > 0 -> writeIORef (_numberOfColours form) nc
      _ -> putStrLn $ "Invalid number of colours input: " ++ numberOfColoursText

    targetStitchesText <- entryGetText targetStitches
    case readMaybe targetStitchesText :: Maybe Int of
      Just ts | ts > 0 -> writeIORef (_targetStitches form) ts
      _ -> putStrLn $ "Invalid target stitches input: " ++ targetStitchesText

    widgetQueueDraw drawingArea

  boxPackStart vbox drawButton PackNatural 0

  let updateDrawingAreaRequestedSize :: StitchConfig -> IO ()
      updateDrawingAreaRequestedSize sc = do
        let (width, height) = calculateCanvasSize sc
        widgetSetSizeRequest drawingArea (fromIntegral width) (fromIntegral height)

  let doDraw :: ValidForm -> Render ()
      doDraw validForm = do
        let (avgGrid, sc) = colourGrid validForm
        liftIO $ updateDrawingAreaRequestedSize sc
        drawGrid sc avgGrid

  -- Connect the "draw" signal to our drawing function
  drawingAreaWidget <- on drawingArea draw $ do
    eitherValidForm <- liftIO $ runExceptT $ validateForm form
    either (\e -> pure ()) doDraw eitherValidForm

  -- Add the drawing area to the vertical box, allowing it to expand
  widgetSetSizeRequest drawingArea 1200 900 -- Temporary big size for now
  boxPackStart vbox scrolledWindow PackGrow 0

  -- --- Export to PNG Button ---
  exportPngButton <- buttonNewWithLabel "Export to PNG"
  widgetSetTooltipText exportPngButton (Just "Save the current drawing as a PNG file.")
  _ <- on exportPngButton buttonActivated $ do
    dialog <-
      fileChooserDialogNew
        (Just "Save Drawing as PNG")
        (Just window)
        FileChooserActionSave
        [ ("Save", ResponseAccept),
          ("Cancel", ResponseCancel)
        ]
    fileChooserSetDoOverwriteConfirmation dialog True
    fileChooserSetCurrentName dialog "stitch_diagram.png"

    response <- dialogRun dialog

    case response of
      ResponseAccept -> do
        mFilename <- fileChooserGetFilename dialog
        case mFilename of
          Just filename -> do
            putStrLn $ "Saving to PNG: " ++ filename
            surface <- createImageSurface FormatRGB24 1200 900 -- This needs to take the actual canvas we drew on!
            renderWith surface $ do
              eitherValidForm <- liftIO $ runExceptT $ validateForm form
              either (\e -> pure ()) doDraw eitherValidForm
            surfaceWriteToPNG surface filename
          Nothing -> putStrLn "No filename selected for PNG export."
      _ -> putStrLn "PNG export cancelled."

    widgetDestroy dialog

  boxPackStart vbox exportPngButton PackNatural 0

  -- Add the vertical box to the window
  containerAdd window vbox

  -- Show all widgets
  widgetShowAll window

  -- Start the GTK+ main event loop
  mainGUI

-- | Creates a horizontal box with a label and an entry for text input.
inputText :: String -> String -> IO (Entry, HBox)
inputText label defaultValue = do
  entry <- entryNew
  entrySetText entry defaultValue
  label <- labelNew (Just label)
  hbox <- hBoxNew False 5
  boxPackStart hbox label PackNatural 0
  boxPackStart hbox entry PackGrow 0
  pure (entry, hbox)

initialForm :: IO Form
initialForm = do
  stitchGauge <- newIORef 2.1
  rowGauge <- newIORef 1.9
  numberOfColours <- newIORef 3
  targetStitches <- newIORef 100
  imageFilePath <- newIORef ""
  pure $ MkForm stitchGauge rowGauge numberOfColours targetStitches imageFilePath

validateForm :: Form -> ExceptT String IO ValidForm
validateForm form = do
  dynamicImage <- ExceptT $ readImage =<< readIORef (_imageFilePath form)
  lift $
    MkValidForm
      <$> readIORef (_stitchGauge form)
      <*> readIORef (_rowGauge form)
      <*> readIORef (_numberOfColours form)
      <*> readIORef (_targetStitches form)
      <*> pure dynamicImage
