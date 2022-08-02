module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Toolbox.Channel (Channel)

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text
