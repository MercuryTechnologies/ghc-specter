module GHCSpecter.Render.SourceView
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( MouseEvent,
    classList,
    height,
    onClick,
    style,
  )
import Control.Lens (at, to, (^.), (^?), _1, _Just)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree, foldTree)
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types (Channel (..))
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Components.TextView qualified as TextView
import GHCSpecter.Server.Types
  ( HasHieState (..),
    HasModuleGraphState (..),
    HasModuleHieInfo (..),
    HasServerState (..),
    Inbox,
    ModuleHieInfo,
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    el,
    hr,
    li,
    pre,
    span,
    text,
    ul,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types
  ( HasSourceViewUI (..),
    SourceViewUI (..),
  )
import GHCSpecter.UI.Types.Event (Event (..))
import GHCSpecter.Util.SourceTree
  ( accumPrefix,
    expandFocusOnly,
  )
import GHCSpecter.Util.Timing (isCompiled)
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)
import Prelude hiding (div, span)

iconText :: Bool -> Text -> Text -> Text -> Widget IHTML MouseEvent
iconText isBordered ico cls txt =
  let iconCls = classList [("fas", True), (ico, True)]
      iconProps = [iconCls, onClick]
      spanProps =
        classList [("icon-text " <> cls, True)] :
        if isBordered
          then [style [("border", "solid")]]
          else []
   in span
        spanProps
        [ span [classList [("icon", True)]] [el "i" iconProps []]
        , span [onClick, style [("font-size", "0.65em")]] [text txt]
        ]

-- | show information on unqualified imports
renderUnqualifiedImports :: ModuleName -> Inbox -> Widget IHTML a
renderUnqualifiedImports modu inbox =
  div [] [pre [] [text rendered]]
  where
    mmsg = inbox ^? at (CheckImports, modu) . _Just
    rendered = maybe "" (\msg -> "\n----- unqualified imports -----\n" <> msg) mmsg

-- | show source code with declaration positions
renderSourceCode :: ModuleHieInfo -> Widget IHTML a
renderSourceCode modHieInfo =
  TextView.render False rendered (fmap (^. _1) topLevelDecls)
  where
    topLevelDecls = getReducedTopLevelDecls modHieInfo
    rendered = modHieInfo ^. modHieSource

renderModuleTree :: SourceViewUI -> ServerState -> Widget IHTML Event
renderModuleTree srcUI ss =
  div
    [ style
        [ ("height", "75vh")
        , ("overflow", "scroll")
        ]
    ]
    [ul [] contents]
  where
    timing = ss ^. serverTiming
    modDrvMap = ss ^. serverDriverModuleRevMap
    mexpandedModu = srcUI ^. srcViewExpandedModule
    expanded = maybe [] (T.splitOn ".") mexpandedModu
    displayedForest =
      ss ^. serverModuleGraphState . mgsModuleForest . to (expandFocusOnly expanded)
    displayedForest' :: [Tree ModuleName]
    displayedForest' =
      fmap (fmap (T.intercalate ".")) . fmap (accumPrefix []) $ displayedForest

    convert :: Widget IHTML Event -> [Widget IHTML Event] -> Widget IHTML Event
    convert x ys
      | null ys = li [] [x]
      | otherwise = li [] [x, ul [] ys]

    contents :: [Widget IHTML Event]
    contents =
      fmap (\tr -> foldTree convert (fmap eachRender tr)) displayedForest'

    eachRender :: ModuleName -> Widget IHTML Event
    eachRender modu =
      let colorTxt
            | isCompiled modDrvMap timing modu = "has-text-success-dark"
            | otherwise = "has-text-grey"
          modItem =
            case mexpandedModu of
              Just modu'
                | modu == modu' -> ExpandModuleEv Nothing <$ iconText True "fa-minus" colorTxt modu
              _ -> ExpandModuleEv (Just modu) <$ iconText False "fa-plus" colorTxt modu
       in modItem

renderCallGraph :: ModuleName -> ServerState -> Widget IHTML a
renderCallGraph modu ss =
  case mgrVis of
    Nothing -> div [] []
    Just grVis -> GraphView.renderGraph (isJust . T.find (== '.')) grVis
  where
    callGraphMap = ss ^. serverHieState . hieCallGraphMap
    mgrVis = M.lookup modu callGraphMap

renderSourceView :: SourceViewUI -> ServerState -> Widget IHTML Event
renderSourceView srcUI ss =
  div
    [ style
        [ ("height", "75vh")
        , ("overflow", "scroll")
        ]
    ]
    contents
  where
    inbox = ss ^. serverInbox
    hie = ss ^. serverHieState
    mexpandedModu = srcUI ^. srcViewExpandedModule
    contents =
      case mexpandedModu of
        Just modu ->
          let mmodHieInfo = hie ^? hieModuleMap . at modu . _Just
              sourcePanel =
                case mmodHieInfo of
                  Nothing -> div [] [pre [] [text "No Hie info"]]
                  Just modHieInfo -> renderSourceCode modHieInfo
           in [ sourcePanel
              , hr []
              , renderCallGraph modu ss
              , hr []
              , renderUnqualifiedImports modu inbox
              ]
        _ -> []

render :: SourceViewUI -> ServerState -> Widget IHTML Event
render srcUI ss =
  div
    [ classList [("columns", True)]
    , style [("overflow", "hidden")]
    , height "100%"
    ]
    [ div
        [ classList [("column is-one-fifths", True)]
        , style [("overflow", "scroll")]
        ]
        [renderModuleTree srcUI ss]
    , div
        [ classList [("column is-four-fifths", True)]
        , style [("overflow", "scroll")]
        ]
        [renderSourceView srcUI ss]
    ]
