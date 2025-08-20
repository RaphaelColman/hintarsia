module GtkApp (mainGtkApp) where

import Codec.Picture (readImage)
import Codec.Picture.Types (DynamicImage)
import Control.Lens (makeLenses)
import Control.Monad (void)
import Data.Either (fromRight)
import Data.IORef
import Data.Maybe
import Debug.Trace
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Linear (V0 (V0))
import Text.Read (readMaybe)
import InputForm
import ImageProcessing (colourGrid)
import CairoDraw (drawGrid)
import Control.Monad.Except

-- | Data type to hold the application's state.
data AppState = AppState
  { _squareSize :: IORef Double,
    _squareXPos :: IORef Double
  }

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

  -- Create mutable references for square properties
  initialSize <- newIORef 100.0
  initialXPos <- newIORef 50.0
  let appState = AppState initialSize initialXPos
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
  -- Entry for Square Size
  sizeEntry <- entryNew
  entrySetText sizeEntry (show (100.0 :: Double)) -- Default value
  sizeLabel <- labelNew (Just "Square Size:")

  -- Entry for Square X Position
  xPosEntry <- entryNew
  entrySetText xPosEntry (show (50.0 :: Double)) -- Default value
  xPosLabel <- labelNew (Just "Square X Position:")

  (stitchGauge, stitchGaugeBox) <- inputText "Stitch gauge stitch/cm" "2.1"
  (rowGauge, rowGaugeBox) <- inputText "Row gauge row/cm" "1.9"
  (numberOfColours, numberOfColoursBox) <- inputText "Number of colours" "3"
  (targetStitches, targetStitchesBox) <- inputText "Target stitches" "100"

  -- Horizontal box for size input
  hboxSize <- hBoxNew False 5
  boxPackStart hboxSize sizeLabel PackNatural 0
  boxPackStart hboxSize sizeEntry PackGrow 0

  -- Horizontal box for X position input
  hboxXPos <- hBoxNew False 5
  boxPackStart hboxXPos xPosLabel PackNatural 0
  boxPackStart hboxXPos xPosEntry PackGrow 0

  -- Add input boxes to the vertical box
  boxPackStart vbox hboxSize PackNatural 0
  boxPackStart vbox hboxXPos PackNatural 0
  boxPackStart vbox stitchGaugeBox PackNatural 0
  boxPackStart vbox rowGaugeBox PackNatural 0
  boxPackStart vbox numberOfColoursBox PackNatural 0
  boxPackStart vbox targetStitchesBox PackNatural 0

  -- --- Drawing Area Section ---
  drawingArea <- drawingAreaNew
  widgetSetSizeRequest drawingArea 400 300

  -- Draw Button
  drawButton <- buttonNewWithLabel "Draw Squares"
  _ <- on drawButton buttonActivated $ do
    -- Read and validate input from sizeEntry
    sizeText <- entryGetText sizeEntry
    case readMaybe sizeText :: Maybe Double of
      Just s | s > 0 -> writeIORef (_squareSize appState) s
      _ -> putStrLn $ "Invalid size input: " ++ sizeText ++ ". Using previous value."

    -- Read and validate input from xPosEntry
    xPosText <- entryGetText xPosEntry
    case readMaybe xPosText :: Maybe Double of
      Just x -> writeIORef (_squareXPos appState) x
      _ -> putStrLn $ "Invalid X position input: " ++ xPosText ++ ". Using previous value."

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

    image <- readImage =<< readIORef (_imageFilePath form)
    let dynamicImage = fromRight undefined image -- Actually handle this better later
    validForm <-
      MkValidForm
        <$> readIORef (_stitchGauge form)
        <*> readIORef (_rowGauge form)
        <*> readIORef (_numberOfColours form)
        <*> readIORef (_targetStitches form)
        <*> pure dynamicImage
    -- This is not good validation. Come back to this later

    let avgGrid = colourGrid validForm
    -- Queue a redraw for the drawing area
    widgetQueueDraw drawingArea

  boxPackStart vbox drawButton PackNatural 0

  -- Connect the "draw" signal to our drawing function
  _ <- on drawingArea draw $ do
    eitherValidForm <- liftIO $ runExceptT $ validateForm form
    either (\e -> pure ()) doDraw eitherValidForm

  -- Add the drawing area to the vertical box, allowing it to expand
  boxPackStart vbox drawingArea PackGrow 0

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
  lift $ MkValidForm
        <$> readIORef (_stitchGauge form)
        <*> readIORef (_rowGauge form)
        <*> readIORef (_numberOfColours form)
        <*> readIORef (_targetStitches form)
        <*> pure dynamicImage

doDraw :: ValidForm -> Render ()
doDraw validForm = do
  let (avgGrid, sc) = colourGrid validForm
  drawGrid sc avgGrid

-- For debug for now
printForm :: Form -> IO ()
printForm form = do
  sg <- readIORef $ _stitchGauge form
  rg <- readIORef $ _rowGauge form
  nc <- readIORef $ _numberOfColours form
  ts <- readIORef $ _targetStitches form
  mfp <- readIORef $ _imageFilePath form
  putStrLn $ "Stitch Gauge: " ++ show sg
  putStrLn $ "Row Gauge: " ++ show rg
  putStrLn $ "Number of Colours: " ++ show nc
  putStrLn $ "Target Stitches: " ++ show ts
  putStrLn $ "Image File Path: " ++ show mfp
