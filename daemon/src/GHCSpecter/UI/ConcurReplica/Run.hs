-- | This module is originated from Concur.Replica.Run. However, we use IHTML instead of HTML.
module GHCSpecter.UI.ConcurReplica.Run
  ( run,
    runDefault,
  )
where

import Concur.Core (SuspendF (Forever, StepBlock, StepIO, StepSTM, StepView), Widget, step)
import Control.Concurrent.STM (atomically)
import Control.Monad.Free (Free (Free, Pure))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHCSpecter.UI.ConcurReplica.Types (IHTML (..), project)
import GHCSpecter.UI.ConcurReplica.WaiHandler qualified as R
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp qualified as W
import Network.WebSockets.Connection (ConnectionOptions, defaultConnectionOptions)
import Replica.VDOM (clientDriver, fireEvent)
import Replica.VDOM.Types
  ( Attr (..),
    DOMEvent (DOMEvent),
    HTML,
    VDOM (..),
  )

customDefaultIndex :: T.Text -> HTML -> HTML
customDefaultIndex title header =
  [ VLeaf "!doctype" (fl [("html", ABool True)]) Nothing
  , VNode
      "html"
      mempty
      Nothing
      [ VNode "head" mempty Nothing $
          [ VLeaf "meta" (fl [("charset", AText "utf-8")]) Nothing
          , VNode "title" mempty Nothing [VText title]
          ]
            <> header
      , VNode
          "body"
          (fl [("style", AText "height: 100vh;")])
          Nothing
          [ VNode
              "script"
              (fl [("language", AText "javascript")])
              Nothing
              [VRawText $ T.decodeUtf8 clientDriver]
          ]
      ]
  ]
  where
    fl = M.fromList

run :: Int -> HTML -> ConnectionOptions -> Middleware -> (R.Context -> Widget IHTML a) -> IO ()
run port index connectionOptions middleware widget =
  W.run port $
    R.app index connectionOptions middleware (step <$> widget) stepWidget

runDefault :: Int -> T.Text -> (R.Context -> Widget IHTML a) -> IO ()
runDefault port title widget =
  W.run port $
    R.app (customDefaultIndex title []) defaultConnectionOptions id (step <$> widget) stepWidget

-- | No need to use this directly if you're using 'run' or 'runDefault'.
stepWidget ::
  R.Context ->
  (R.Context -> Free (SuspendF IHTML) a) ->
  IO
    ( Maybe
        ( IHTML
        , R.Context -> Free (SuspendF IHTML) a
        , R.Event -> Maybe (IO ())
        )
    )
stepWidget ctx v = case v ctx of
  Pure _ -> pure Nothing
  Free (StepView new next) ->
    pure $
      Just
        ( new
        , const next
        , \event -> fireEvent (project new) (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event)
        )
  Free (StepIO io next) ->
    io >>= stepWidget ctx . \r _ -> next r
  Free (StepBlock io next) ->
    io >>= stepWidget ctx . \r _ -> next r
  Free (StepSTM stm next) ->
    atomically stm >>= stepWidget ctx . \r _ -> next r
  Free Forever ->
    pure Nothing
