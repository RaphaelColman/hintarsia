module GtkApp (mainGtkApp) where

import Graphics.UI.Gtk          -- Main GTK+ bindings
import Graphics.Rendering.Cairo -- For drawing in the drawing area
import Control.Monad (void)     -- For ignoring return values

-- | The main GTK+ application function.
mainGtkApp :: IO ()
mainGtkApp = do
    -- Initialize GTK+
    void initGUI

    -- Create the main window
    window <- windowNew
    set window [ windowTitle := "Hello GTK+!"
               , windowDefaultWidth := 300
               , windowDefaultHeight := 100 ]

    -- Connect the destroy event to quit the GTK+ main loop
    _ <- on window objectDestroy mainQuit

    -- vertical box to arrange widgets
    vbox <- vBoxNew False 5

    fileChooserButton <- buttonNewWithLabel "Select file"
    _ <- on fileChooserButton buttonActivated $ do
        -- Create a file chooser dialog
        dialog <- fileChooserDialogNew (Just "Choose a file")
                                        (Just window)
                                        FileChooserActionOpen
                                        [("Cancel", ResponseCancel),
                                         ("Open", ResponseAccept)]
        response <- dialogRun dialog
        case response of
            ResponseAccept -> do
                -- Get the selected file name
                filename <- fileChooserGetFilename dialog
                putStrLn $ "Selected file: " ++ show filename
            _ -> return ()
        widgetDestroy dialog
    
    boxPackStart vbox fileChooserButton PackNatural 0

    -- Create a label with "Hello World!" text
    helloLabel <- labelNew (Just "Hello World!")

    -- Add the label to the vertical box
    boxPackStart vbox helloLabel PackNatural 0

    -- Add the bo to the the window
    containerAdd window vbox

    -- Show all widgets (in this case, just the label and the window)
    widgetShowAll window

    -- Start the GTK+ main event loop
    mainGUI
