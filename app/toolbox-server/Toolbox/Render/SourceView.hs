module Toolbox.Render.SourceView
  ( render,
    splitLineColumn,
    tempWorker,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( MouseEvent,
    classList,
    div,
    el,
    hr,
    li,
    onClick,
    pre,
    span,
    style,
    text,
    ul,
  )
import Control.Lens (at, to, (^.), (^..), (^?), _1, _Just)
import Control.Monad.Trans.State (State, get, put, runState)
import Data.Foldable qualified as F
import Data.Function (on)
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
    HasDeclRow' (..),
    HasHieState (..),
    HasModuleHieInfo (..),
    HasServerState (..),
    HasSourceViewUI (..),
    Inbox,
    ModuleHieInfo,
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

tempWorker :: ServerState -> IO ()
tempWorker ss = do
  case mmodHieInfo of
    Nothing -> putStrLn "No Hie file"
    Just modHieInfo -> do
      let topLevelDecls = getTopLevelDecls modHieInfo
      F.traverse_ print topLevelDecls
  where
    hie = ss ^. serverHieState
    modu = "AWS.Types"
    mmodHieInfo = hie ^? hieModuleMap . at modu . _Just

isContainedIn :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
(s1, e1) `isContainedIn` (s2, e2) = s1 >= s2 && e1 <= e2

filterTopLevel :: (Show a) => [(((Int, Int), (Int, Int)), a)] -> [(((Int, Int), (Int, Int)), a)]
filterTopLevel items = go [] items
  where
    go ys [] = ys
    go ys (x : xs) =
      let ys' = L.nubBy ((==) `on` fst) $ L.sortBy (compare `on` fst) $ go' ys x
       in go ys' xs
    go' [] x = [x]
    go' zs@(y : ys) x
      | fst x `isContainedIn` fst y = zs
      | fst y `isContainedIn` fst x = x : go' ys x
      | otherwise = y : go' ys x

getTopLevelDecls :: ModuleHieInfo -> [(((Int, Int), (Int, Int)), Text)]
getTopLevelDecls modHieInfo = sortedTopLevelDecls
  where
    extract decl =
      let spos = (decl ^. decl'SLine, decl ^. decl'SCol)
          epos = (decl ^. decl'ELine, decl ^. decl'ECol)
          name = decl ^. decl'NameOcc
       in ((spos, epos), name)
    decls = modHieInfo ^.. modHieDecls . traverse . to extract
    topLevelDecls = filterTopLevel decls
    sortedTopLevelDecls = L.sortBy (compare `on` (^. _1)) topLevelDecls

breakSourceText :: ModuleHieInfo -> [Text]
breakSourceText modHieInfo = txts ++ [txt]
  where
    src = modHieInfo ^. modHieSource
    topLevelDecls = getTopLevelDecls modHieInfo
    (txts, (_, txt)) = runState (traverse (splitLineColumn . (^. _1 . _1)) topLevelDecls) ((1, 1), src)

-- | show source code with declaration positions
renderSourceCode :: ModuleHieInfo -> Widget HTML a
renderSourceCode modHieInfo = pre [] rendered
  where
    theicon =
      span
        [style [("position", "relative"), ("width", "0"), ("height", "0")]]
        [ span
            [ classList [("icon", True)]
            , style [("position", "absolute"), ("top", "-16px"), ("left", "-9px")]
            ]
            [el "i" [classList [("fas fa-long-arrow-alt-down has-text-primary", True)]] []]
        ]
    rendered =
      L.intersperse theicon $ fmap text $ breakSourceText modHieInfo

-- | list decls
renderDecls :: ModuleHieInfo -> Widget HTML a
renderDecls modHieInfo = pre [] (fmap (text . (<> "\n") . T.pack . show) topLevelDecls)
  where
    topLevelDecls = getTopLevelDecls modHieInfo

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
          modinfo =
            case mexpandedModu of
              Just modu'
                | modu == modu' ->
                    let mmodHieInfo = hie ^? hieModuleMap . at modu . _Just
                        sourcePanel =
                          case mmodHieInfo of
                            Nothing -> div [] [pre [] [text "No Hie info"]]
                            Just modHieInfo ->
                              div
                                [classList [("columns", True)]]
                                [ div [classList [("column is-half", True)]] [renderSourceCode modHieInfo]
                                , div [classList [("column is-half", True)]] [renderDecls modHieInfo]
                                ]

                        expanded = [sourcePanel, hr [], renderUnqualifiedImports modu inbox]
                     in (ExpandModuleEv Nothing <$ iconText "fa-minus" colorTxt modu) : expanded
              _ ->
                [ExpandModuleEv (Just modu) <$ iconText "fa-plus" colorTxt modu]
       in li [] modinfo
