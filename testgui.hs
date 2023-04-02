import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.BackgroundContainer
import Graphics.UI.Gtk.Board.BoardLink
import GtkPegSolitaire

main :: IO ()
main = do
  -- View

  -- Initialise Gtk
  _ <- initGUI

  -- Create interface
  window  <- windowNew
  bgBin   <- backgroundContainerNewWithPicture "Free-Background-3.jpg"
  align   <- alignmentNew 0.5 0.5 0 0

  -- Create game and board
  game    <- gtkGame
  board   <- attachGameRules game

  -- Add hierarchy of widgets to window
  containerAdd align board
  containerAdd bgBin align
  containerAdd window bgBin

  -- Set window size
  widgetSetSizeRequest window 400 300

  -- Close program if window is closed
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Launch program with the main window
  widgetShowAll window
  mainGUI
