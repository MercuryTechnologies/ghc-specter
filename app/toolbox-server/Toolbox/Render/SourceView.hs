module Toolbox.Render.SourceView
  ( render,
    splitLineColumn,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( MouseEvent,
    classList,
    div,
    el,
    li,
    onClick,
    pre,
    span,
    style,
    text,
    ul,
  )
import Control.Lens (at, to, (^.), (^?), _Just)
import Control.Monad.Trans.State (State, get, put, runState)
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Replica.VDOM.Types (HTML)
import Toolbox.Channel
  ( Channel (..),
    ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
    Timer (..),
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

iconText :: Text -> Text -> Text -> Widget HTML MouseEvent
iconText ico cls txt =
  let iconCls = classList [("fas", True), (ico, True)]
      iconProps = [iconCls, onClick]
   in span
        [classList [("icon-text " <> cls, True)]]
        [ span [classList [("icon", True)]] [el "i" iconProps []]
        , span [onClick] [text txt]
        ]

-- | show information on unqualified imports
renderUnqualifiedImports :: ModuleName -> Inbox -> Widget HTML a
renderUnqualifiedImports modu inbox =
  div [] [pre [] [text rendered]]
  where
    mmsg = inbox ^? at (CheckImports, modu) . _Just
    rendered = maybe "" (\msg -> "\n----- unqualified imports -----\n" <> msg) mmsg

-- | splitter based on line and column
-- line and col are 1-based.
splitLineColumn :: (Int, Int) -> State ((Int, Int), Text) Text
splitLineColumn (lin, col) = do
  ((currLin, currCol), remainingTxt) <- get
  let lineSplittedTxt = T.lines remainingTxt
      (linesBefore, linesAfter) = splitAt (lin - currLin) lineSplittedTxt
      ((txtInBreakLineBefore, txtInBreakLineAfter), linesAfterBreakLine) =
        case linesAfter of
          [] -> (("", ""), [])
          breakLine : xs ->
            if null linesBefore
              then (T.splitAt (col - currCol) breakLine, xs)
              else (T.splitAt (col - 1) breakLine, xs)
      txtBefore = T.intercalate "\n" (linesBefore ++ [txtInBreakLineBefore])
      txtAfter = T.intercalate "\n" (txtInBreakLineAfter : linesAfterBreakLine)
  put ((lin, col), txtAfter)
  pure txtBefore

-- | show source code
renderSourceCode :: ModuleName -> HieState -> Widget HTML a
renderSourceCode modu hie =
  div [] [pre [] rendered]
  where
    msrc = hie ^? hieModuleMap . at modu . _Just . modHieSource
    theicon =
      span
        [style [("position", "relative"), ("width", "0"), ("height", "0")]]
        [ span
            [ classList [("icon", True)]
            , style [("position", "absolute"), ("top", "-16px"), ("left", "-9px")]
            ]
            [el "i" [classList [("fas fa-long-arrow-alt-down", True)]] []]
        ]
    rendered =
      L.intersperse theicon $
        fmap text $ maybe [] (T.chunksOf 20) msrc

-- | Top-level render function for the Source View tab
render :: SourceViewUI -> ServerState -> Widget HTML Event
render srcUI ss =
  ul [] $ map eachRender allModules
  where
    inbox = ss ^. serverInbox
    hie = ss ^. serverHieState
    timing = ss ^. serverTiming
    -- NOTE: We do not want to have lens dependency for the plugin.
    allModules = ss ^. serverSessionInfo . to (F.toList . mginfoModuleNameMap . sessionModuleGraph)
    mexpandedModu = srcUI ^. srcViewExpandedModule

    eachRender :: ModuleName -> Widget HTML Event
    eachRender modu =
      let isCompiled = isJust (timing ^? at modu . _Just . to timerEnd . _Just)
          colorTxt
            | isCompiled = "has-text-success-dark"
            | otherwise = "has-text-grey"
          modinfo
            | mexpandedModu == Just modu =
                [ ExpandModuleEv Nothing <$ iconText "fa-minus" colorTxt modu
                , -- , renderUnqualifiedImports modu inbox
                  renderSourceCode modu hie
                ]
            | otherwise =
                [ExpandModuleEv (Just modu) <$ iconText "fa-plus" colorTxt modu]
       in li [] modinfo
