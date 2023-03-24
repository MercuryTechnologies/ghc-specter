{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base (AttrOp ((:=)), new, on)
import GI.Gtk qualified as Gtk

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  _ <- mainWindow `on` #destroy $ Gtk.mainQuit
  #showAll mainWindow
  Gtk.main

