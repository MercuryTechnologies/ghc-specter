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
import Concur.Replica.DOM.Props qualified as DP
import Control.Lens (at, to, (^.), (^?), _1, _Just)
import Data.Bifunctor (first)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), foldTree)
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( Channel (..),
    SessionInfo (..),
  )
import GHCSpecter.Data.GHC.Hie (HasModuleHieInfo (..), ModuleHieInfo)
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Components.TextView qualified as TextView
import GHCSpecter.Server.Types
  ( HasHieState (..),
    HasModuleGraphState (..),
    HasServerState (..),
    Inbox,
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    hr,
    input,
    li,
    pre,
    span,
    text,
    ul,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Constants (widgetHeight)
import GHCSpecter.UI.Types
  ( HasSourceViewUI (..),
    SourceViewUI (..),
  )
import GHCSpecter.UI.Types.Event (Event (..))
import GHCSpecter.Util.SourceTree
  ( accumPrefix,
    expandFocusOnly,
    markLeaf,
  )
import GHCSpecter.Util.Timing (isModuleCompilationDone)
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)
import Prelude hiding (div, span)

expandableText :: Bool -> Bool -> Text -> Text -> Widget IHTML MouseEvent
expandableText isBordered isExpandable cls txt =
  let txt'
        | not isBordered && isExpandable = txt <> " ... "
        | otherwise = txt
      spanProps =
        classList [("expandable " <> cls, True)] :
        if isBordered
          then [style [("border", "solid")]]
          else []
   in span (onClick : spanProps) [text txt']

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
        [ ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
        , ("overflow", "scroll")
        ]
    ]
    [ul [classList [("tree", True)]] contents]
  where
    timing = ss ^. serverTiming
    drvModMap = ss ^. serverDriverModuleMap
    mexpandedModu = srcUI ^. srcViewExpandedModule
    expanded = maybe [] (T.splitOn ".") mexpandedModu
    displayedForest =
      ss ^. serverModuleGraphState . mgsModuleForest . to (expandFocusOnly expanded . fmap markLeaf)
    displayedForest' :: [Tree (ModuleName, Bool)]
    displayedForest' =
      fmap (fmap (first (T.intercalate "."))) . fmap (accumPrefix []) $ displayedForest

    convert :: Widget IHTML Event -> [Widget IHTML Event] -> Widget IHTML Event
    convert x ys
      | null ys = li [] [x]
      | otherwise = li [] [x, ul [] ys]

    contents :: [Widget IHTML Event]
    contents = fmap renderTree displayedForest'
      where
        renderTree = foldTree convert . fmap renderNode
    renderNode :: (ModuleName, Bool) -> Widget IHTML Event
    renderNode (modu, b) =
      let colorTxt
            | isModuleCompilationDone drvModMap timing modu = "has-text-green"
            | otherwise = "has-text-black"
          breakpointCheck =
            input
              [ DP.type_ "checkbox"
              , DP.name "breakpoint"
              , DP.checked True
              , style [("width", "8px"), ("height", "8px")]
              ]
          modItem =
            case mexpandedModu of
              Just modu'
                | modu == modu' ->
                    span
                      []
                      [ ExpandModuleEv Nothing
                          <$ expandableText True (not b) colorTxt modu
                      , breakpointCheck
                      ]
              _ ->
                span
                  []
                  [ ExpandModuleEv (Just modu)
                      <$ expandableText False (not b) colorTxt modu
                  , breakpointCheck
                  ]
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
        [ ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
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
        [ classList [("column box is-one-fifths", True)]
        , style [("overflow", "scroll")]
        ]
        [renderModuleTree srcUI ss]
    , div
        [ classList [("column box is-four-fifths", True)]
        , style [("overflow", "scroll")]
        ]
        [renderSourceView srcUI ss]
    ]
