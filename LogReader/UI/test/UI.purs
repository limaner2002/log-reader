module LogReader.UI where

import Prelude
import Data.Array
import Control.Monad.Eff.Var (($=), get)
import Control.Bind
import Control.Monad.Eff.Console (log)
import Data.Either
import Data.Monoid

import LogReader.Bootstrap
import LogReader.Data
import WebSocket

main = do
  Connection socket <- newWebSocket (URL "ws://localhost:3000/log/Tmp") []

  socket.onmessage $= \event -> do

         let received = runMessage (runMessageEvent event)
             eFiles = eitherDecode received :: Either String LogFiles
         case eFiles of
           Left msg -> log msg
           Right (LogFiles o) ->
               log $ Text.Smolder.Renderer.String.render $
                   buildVerticalNav $ NavBar
                       { title: (Pills o.logFiles)
                       , content: (mempty :: String)
                       }
