module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)

type ChanModule = (Text, Text)

type Inbox = Map ChanModule Text
