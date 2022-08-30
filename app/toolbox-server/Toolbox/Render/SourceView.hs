module Toolbox.Render.SourceView
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( MouseEvent,
    classList,
    el,
    li,
    onClick,
    pre,
    span,
    text,
    ul,
  )
import Control.Lens ((^.))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Replica.VDOM.Types (HTML)
import Toolbox.Channel (Channel (..))
import Toolbox.Server.Types
  ( Event (..),
    HasServerState (..),
    HasSourceViewUI (..),
    ServerState (..),
    SourceViewUI (..),
    type ChanModule,
  )
import Prelude hiding (div, span)

iconText :: Text -> Text -> Widget HTML MouseEvent
iconText ico txt =
  let iconCls = classList [("fas", True), (ico, True)]
      iconProps = [iconCls, onClick]
   in span
        [classList [("icon-text", True)]]
        [ span [classList [("icon", True)]] [el "i" iconProps []]
        , span [onClick] [text txt]
        ]

-- | Top-level render function for the Source View tab
render :: SourceViewUI -> ServerState -> Widget HTML Event
render srcUI ss =
  ul [] $ map eachRender filtered
  where
    inbox = ss ^. serverInbox
    mexpandedModu = srcUI ^. srcViewExpandedModule
    filtered =
      M.toList $
        M.filterWithKey (\(c, _) _ -> c == CheckImports) inbox

    eachRender :: (ChanModule, Text) -> Widget HTML Event
    eachRender ((_, modu), v) =
      let modinfo
            | mexpandedModu == Just modu =
                [ ExpandModuleEv Nothing <$ iconText "fa-minus" modu
                , pre [] [text v]
                ]
            | otherwise =
                [ExpandModuleEv (Just modu) <$ iconText "fa-plus" modu]
       in li [] modinfo
