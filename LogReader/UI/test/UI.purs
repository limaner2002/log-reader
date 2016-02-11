module LogReader.UI where

import Prelude
import Data.Array
import Control.Monad.Eff.Var (($=), get)
import Control.Bind
import Control.Monad.Eff.Console (log)

import LogReader.Bootstrap
import WebSocket

data LogFiles = LogFiles (Array String)

main = do
  Connection socket <- newWebSocket (URL "ws://10.203.50.211:3000/appianLog/JBoss") []

  socket.onmessage $= \event -> do

         let received = runMessage (runMessageEvent event)

         log $ "received '" ++ received ++ "'"