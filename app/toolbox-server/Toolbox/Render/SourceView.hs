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
import Control.Lens (at, to, (^.), (^?), _Just)
import Data.Foldable qualified as F
import Data.Text (Text)
import Replica.VDOM.Types (HTML)
import Toolbox.Channel
  ( Channel (..),
    ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
  )
import Toolbox.Server.Types
  ( Event (..),
    HasHieState (..),
    HasModuleHieInfo (..),
    HasServerState (..),
    HasSourceViewUI (..),
    HieState (..),
    Inbox,
    ServerState (..),
    SourceViewUI (..),
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

renderModuleContent :: ModuleName -> HieState -> Inbox -> Text
renderModuleContent modu hie inbox =
  maybe "" (\msg -> "\n----- unqualified imports -----\n" <> msg) mmsg
    <> maybe "" (\src -> "\n----- source -----\n" <> src) msrc
  where
    mmsg = inbox ^? at (CheckImports, modu) . _Just
    msrc = hie ^? hieModuleMap . at modu . _Just . modHieSource

-- | Top-level render function for the Source View tab
render :: SourceViewUI -> ServerState -> Widget HTML Event
render srcUI ss =
  ul [] $ map eachRender allModules
  where
    inbox = ss ^. serverInbox
    hie = ss ^. serverHieState
    -- NOTE: We do not want to have lens dependency for the plugin.
    allModules = ss ^. serverSessionInfo . to (F.toList . mginfoModuleNameMap . sessionModuleGraph)
    mexpandedModu = srcUI ^. srcViewExpandedModule

    eachRender :: ModuleName -> Widget HTML Event
    eachRender modu =
      let modinfo
            | mexpandedModu == Just modu =
                [ ExpandModuleEv Nothing <$ iconText "fa-minus" modu
                , pre [] [text (renderModuleContent modu hie inbox)]
                ]
            | otherwise =
                [ExpandModuleEv (Just modu) <$ iconText "fa-plus" modu]
       in li [] modinfo
