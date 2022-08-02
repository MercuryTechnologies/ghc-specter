{-# OPTIONS_GHC -Werror #-}

module Toolbox.Render
  ( ChanModule,
    Inbox,
    render,
  )
where

import Concur.Core (Widget)
import Concur.Replica (button, div, el, onClick, pre, text)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Channel (Channel (..))
import Toolbox.Server.Types (type ChanModule, type Inbox)
import Prelude hiding (div)

hrule :: Widget HTML a
hrule = el "hr" [] []

renderChannel :: Channel -> Inbox -> Widget HTML a
renderChannel chan m =
  div [] $ map eachRender filtered
  where
    filtered = M.toList $ M.filterWithKey (\(c, _) _ -> chan == c) m

    eachRender :: (ChanModule, Text) -> Widget HTML a
    eachRender ((_, modu), v) =
      div
        []
        [ text ("chan: " <> T.pack (show chan) <> ", module: " <> modu)
        , pre [] [text v]
        , pre [] [text "-----------"]
        ]

render ::
  (Channel, (Int, Inbox)) ->
  Widget HTML (Channel, (Int, Inbox))
render (chan, (i, m)) = do
  let topPanel =
        div
          []
          [ pre [] [text $ "Current channel = " <> T.pack (show chan)]
          , CheckImports <$ button [onClick] [text "CheckImports"]
          , Trivial <$ button [onClick] [text "Trivial"]
          ]
      (mainPanel, bottomPanel)
        | i == 0 = (pre [] [text "No GHC process yet"], div [] [])
        | otherwise =
            ( div [] [renderChannel chan m]
            , div [] [text $ "message: " <> T.pack (show i)]
            )
  chan' <- div [] [topPanel, hrule, mainPanel, hrule, bottomPanel]
  pure (chan', (i, m))
