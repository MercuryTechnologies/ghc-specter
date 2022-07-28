{-# OPTIONS_GHC -Werror #-}

module Toolbox.Render
  ( ChanModule,
    Inbox,
    render,
  )
where

import Concur.Core (Widget)
import Concur.Replica (div, pre, text)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Server.Types (type ChanModule, type Inbox)
import Prelude hiding (div)

renderInbox :: Inbox -> Widget HTML a
renderInbox m =
  div [] $ map eachRender $ M.toList m
  where
    eachRender :: (ChanModule, Text) -> Widget HTML a
    eachRender ((chan, modu), v) =
      div
        []
        [ text ("chan: " <> chan <> ", module: " <> modu)
        , pre [] [text v]
        , pre [] [text "-----------"]
        ]

render ::
  (Int, Inbox) ->
  Widget HTML (Int, Inbox)
render (i, m)
  | i == 0 = pre [] [text "No GHC process yet"]
  | otherwise = 
    div [] [text (T.pack (show i)), renderInbox m]
