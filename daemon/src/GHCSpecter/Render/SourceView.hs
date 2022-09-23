module GHCSpecter.Render.SourceView
  ( render,
    splitLineColumn,
    test,
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
import Control.Lens (at, to, (^.), (^..), (^?), _1, _Just)
import Control.Monad (guard)
import Control.Monad.Trans.State (State, get, put, runState)
import Data.Foldable (for_)
import Data.Function (on)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree, foldTree)
import GHCSpecter.Channel
  ( Channel (..),
    ModuleName,
    getEndTime,
  )
import GHCSpecter.Render.Components.TextView qualified as TextView (render)
import GHCSpecter.Server.Types
  ( HasDeclRow' (..),
    HasHieState (..),
    HasModuleGraphState (..),
    HasModuleHieInfo (..),
    HasRefRow' (..),
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

sliceText :: (Int, Int) -> (Int, Int) -> State ((Int, Int), Text) Text
sliceText start end = do
  _ <- splitLineColumn start
  splitLineColumn end

findText :: Text -> Text -> Maybe (Int, Int)
findText needle haystick = do
  let (searched, remaining) = T.breakOn needle haystick
  guard (not (T.null remaining))
  let ls = T.lines searched
  case NE.nonEmpty ls of
    Nothing ->
      Just (0, 0)
    Just ls' ->
      Just (NE.length ls' - 1, T.length (NE.last ls'))

addRowCol :: (Int, Int) -> (Int, Int) -> (Int, Int)
addRowCol (i, j) (di, dj)
  | di == 0 = (i, j + dj)
  | otherwise = (i + di, dj)

reduceDeclRange :: Text -> ((Int, Int), (Int, Int)) -> Text -> Maybe ((Int, Int), (Int, Int))
reduceDeclRange src (start, end) needle =
  let (sliced, _) = runState (sliceText start end) ((1, 1), src)
      mdidj = findText needle sliced
      indexFromStart didj =
        let startOfNeedle = addRowCol start didj
            endOfNeedle = addRowCol startOfNeedle (0, T.length needle - 1)
         in (startOfNeedle, endOfNeedle)
   in fmap indexFromStart mdidj

getReducedTopLevelDecls :: ModuleHieInfo -> [(((Int, Int), (Int, Int)), Text)]
getReducedTopLevelDecls modHieInfo =
  mapMaybe
    (\((start, end), decl) -> (,decl) <$> reduceDeclRange src (start, end) decl)
    topLevelDecls
  where
    src = modHieInfo ^. modHieSource
    topLevelDecls = getTopLevelDecls modHieInfo

breakSourceText :: ModuleHieInfo -> [Text]
breakSourceText modHieInfo = txts ++ [txt]
  where
    src = modHieInfo ^. modHieSource
    topLevelDecls = getTopLevelDecls modHieInfo
    (txts, (_, txt)) = runState (traverse (splitLineColumn . (^. _1 . _1)) topLevelDecls) ((1, 1), src)

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
      let isCompiled = isJust (timing ^? at modu . _Just . to getEndTime . _Just)
          colorTxt
            | isCompiled = "has-text-success-dark"
            | otherwise = "has-text-grey"
          modItem =
            case mexpandedModu of
              Just modu'
                | modu == modu' -> ExpandModuleEv Nothing <$ iconText True "fa-minus" colorTxt modu
              _ -> ExpandModuleEv (Just modu) <$ iconText False "fa-plus" colorTxt modu
       in modItem

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
           in [sourcePanel, hr [], renderUnqualifiedImports modu inbox]
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

test :: ServerState -> IO ()
test ss = do
  putStrLn "test"
  let modName = "Metrics"
      mmodHieInfo = ss ^? serverHieState . hieModuleMap . at modName . _Just
  for_ mmodHieInfo $ \modHieInfo -> do
    let decls = getTopLevelDecls modHieInfo
        allRefs = modHieInfo ^.. modHieRefs . traverse
    for_ decls $ \((declStart, declEnd), name) -> do
      putStrLn "------"
      print (name, (declStart, declEnd))
      let isHiddenSymbol r =
            "$" `T.isPrefixOf` (r ^. ref'NameOcc)
          isSelf r =
            r ^. ref'NameOcc == name && r ^. ref'NameMod == modName
          isDepOn r =
            let refStart = (r ^. ref'SLine, r ^. ref'SCol)
                refEnd = (r ^. ref'ELine, r ^. ref'ECol)
             in (refStart, refEnd) `isContainedIn` (declStart, declEnd)

      let depRefs =
            filter (\r -> isDepOn r && not (isSelf r) && not (isHiddenSymbol r)) allRefs
          depRefNames = L.nub $ L.sort $ fmap (\r -> (r ^. ref'NameMod, r ^. ref'NameOcc)) depRefs
      print depRefNames
