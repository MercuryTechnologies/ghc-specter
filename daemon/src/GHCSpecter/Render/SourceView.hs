module GHCSpecter.Render.SourceView
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( MouseEvent,
    classList,
    height,
    onChange,
    onClick,
    style,
  )
import Concur.Replica.DOM.Props qualified as DP
import Control.Lens (at, to, (^.), (^?), _1, _Just)
import Data.Bifunctor (first)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), foldTree)
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( Channel (..),
    SessionInfo (..),
  )
import GHCSpecter.Data.GHC.Hie
  ( HasDeclRow' (..),
    HasModuleHieInfo (..),
    ModuleHieInfo,
  )
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Components.TextView qualified as TextView
import GHCSpecter.Render.Util (divClass)
import GHCSpecter.Server.Types
  ( HasHieState (..),
    HasModuleGraphState (..),
    HasServerState (..),
    HasTimingState (..),
    Inbox,
    ServerState (..),
    SupplementaryView (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    el,
    input,
    li,
    nav,
    pre,
    script,
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
import GHCSpecter.UI.Types.Event
  ( Event (..),
    SourceViewEvent (..),
  )
import GHCSpecter.Util.SourceTree
  ( accumPrefix,
    expandFocusOnly,
    markLeaf,
  )
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

renderModuleTree :: SourceViewUI -> ServerState -> Widget IHTML SourceViewEvent
renderModuleTree srcUI ss =
  div
    [ style
        [ ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
        , ("overflow", "scroll")
        ]
    ]
    [ul [classList [("tree", True)]] contents]
  where
    timing = ss ^. serverTiming . tsTimingMap
    drvModMap = ss ^. serverDriverModuleMap
    mexpandedModu = srcUI ^. srcViewExpandedModule
    expanded = maybe [] (T.splitOn ".") mexpandedModu
    displayedForest =
      ss ^. serverModuleGraphState . mgsModuleForest . to (expandFocusOnly expanded . fmap markLeaf)
    displayedForest' :: [Tree (ModuleName, Bool)]
    displayedForest' =
      fmap (fmap (first (T.intercalate "."))) . fmap (accumPrefix []) $ displayedForest
    breakpoints = ss ^. serverModuleBreakpoints

    convert :: Widget IHTML e -> [Widget IHTML e] -> Widget IHTML e
    convert x ys
      | null ys = li [] [x]
      | otherwise = li [] [x, ul [] ys]

    contents :: [Widget IHTML SourceViewEvent]
    contents = fmap renderTree displayedForest'
      where
        renderTree = foldTree convert . fmap renderNode
    renderNode :: (ModuleName, Bool) -> Widget IHTML SourceViewEvent
    renderNode (modu, b) =
      let colorTxt
            | isModuleCompilationDone drvModMap timing modu = "has-text-green"
            | otherwise = "has-text-black"
          hasBreakpoint = modu `elem` breakpoints
          breakpointCheck =
            input
              [ SetBreakpoint modu (not hasBreakpoint) <$ onChange
              , DP.type_ "checkbox"
              , DP.name "breakpoint"
              , DP.checked hasBreakpoint
              , style [("width", "8px"), ("height", "8px")]
              ]
          modItem =
            case mexpandedModu of
              Just modu'
                | modu == modu' ->
                    span
                      []
                      [ UnselectModule
                          <$ expandableText True (not b) colorTxt modu
                      , breakpointCheck
                      ]
              _ ->
                span
                  []
                  [ SelectModule modu
                      <$ expandableText False (not b) colorTxt modu
                  , breakpointCheck
                  ]
       in modItem

renderSuppView :: SupplementaryView -> Widget IHTML a
renderSuppView (SuppViewCallgraph grVis) =
  div
    [ style
        [ ("overflow", "scroll")
        , ("height", "100%")
        ]
    ]
    [GraphView.renderGraph (isJust . T.find (== '.')) grVis]
renderSuppView _ = div [] []

renderSuppViewPanel :: ModuleName -> SourceViewUI -> ServerState -> Widget IHTML Event
renderSuppViewPanel modu srcUI ss =
  div [style [("overflow", "hidden"), ("height", "100%")]] [suppViewTabs, suppViewContents]
  where
    suppViews = fromMaybe [] (M.lookup modu (ss ^. serverSuppView))
    msuppView = do
      tab <- srcUI ^. srcViewSuppViewTab
      suppView <- L.lookup tab suppViews
      pure suppView

    navbarMenu = divClass "navbar-menu" []
    navbarStart = divClass "navbar-start" []
    navItem (k, tab) =
      let isActive = Just k == srcUI ^. srcViewSuppViewTab
          clss
            | isActive = ["navbar-item", "is-tab", "supp-tab", "is-active"]
            | otherwise = ["navbar-item", "is-tab", "supp-tab"]
          cls = classList $ map (\tag -> (tag, True)) clss
       in el
            "a"
            [cls, SourceViewEv (SourceViewTab k) <$ onClick]
            [text tab]

    suppViewTabs =
      nav
        [classList [("navbar", True)]]
        [navbarMenu [navbarStart (fmap (\(t, _) -> navItem (t, t)) suppViews)]]
    suppViewContents =
      case msuppView of
        Nothing -> div [] []
        Just suppView -> renderSuppView suppView

renderSourceView :: SourceViewUI -> ServerState -> Widget IHTML Event
renderSourceView srcUI ss =
  divClass
    "columns"
    [ style
        [ ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
        , ("overflow", "hidden")
        ]
    ]
    contents
  where
    hie = ss ^. serverHieState
    mexpandedModu = srcUI ^. srcViewExpandedModule

    -- This is a hack. Property update should be supported by concur-replica.
    -- TODO: implement prop update in internalized concur-replica.
    scriptContent =
      script
        []
        [ text
            "var me1 = document.currentScript;\n\
            \var myParent1 = me1.parentElement;\n\
            \console.log(myParent1);\n\
            \var top0 = myParent1.getAttribute(\"myval\");\n\
            \myParent1.scrollTop = top0;\n\
            \var config1 = {attributes: true, childList: false, subtree: false, characterData: false};\n\
            \var callback1 = (mutationList, observer) => {\n\
            \      var top = myParent1.getAttribute(\"myval\");\n\
            \      console.log (\"callback called\" + top);\n\
            \      myParent1.scrollTop = top;\n\
            \    };\n\
            \var observer1 = new MutationObserver(callback1);\n\
            \observer1.observe(myParent1, config);\n"
        ]
    contents =
      case mexpandedModu of
        Just modu ->
          let mmodHieInfo = hie ^? hieModuleMap . at modu . _Just
              (sourcePanel, myval) =
                case mmodHieInfo of
                  Nothing -> (div [] [pre [] [text "No Hie info"]], "")
                  Just modHieInfo ->
                    let srcPanel = renderSourceCode modHieInfo
                        val =
                          case srcUI ^. srcViewFocusedBinding of
                            Nothing -> ""
                            Just sym ->
                              let mline =
                                    fmap (^. decl'SLine) $
                                      L.find (\d -> d ^. decl'NameOcc == sym) $
                                        modHieInfo ^. modHieDecls
                               in maybe "" (T.pack . show . TextView.topOfBox) mline
                     in (srcPanel, val)
           in [ divClass
                  "column box is-half"
                  [ style [("overflow", "scroll")]
                  , DP.textProp "myval" myval
                  ]
                  [scriptContent, sourcePanel]
              , divClass
                  "column box is-half"
                  [style [("overflow", "scroll")]]
                  [renderSuppViewPanel modu srcUI ss]
              ]
        _ -> []

render :: SourceViewUI -> ServerState -> Widget IHTML Event
render srcUI ss =
  divClass
    "columns"
    [ style [("overflow", "hidden")]
    , height "100%"
    ]
    [ divClass
        "column box is-one-fifths"
        [style [("overflow", "scroll")]]
        [SourceViewEv <$> renderModuleTree srcUI ss]
    , divClass
        "column box is-four-fifths"
        [style [("overflow", "scroll")]]
        [renderSourceView srcUI ss]
    ]
