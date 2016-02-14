module LogReader.UI where

import Prelude
import Data.Array
import Data.Tuple
import Control.Monad.Eff.Var (($=), get)
import Control.Bind
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Data.Either
import Data.Monoid
import DOM.Event.Types
import qualified DOM.Event.EventTarget as ET
import qualified DOM.Event.EventTypes as ET

import LogReader.Bootstrap
import LogReader.Data
import WebSocket
import Signal.Channel
import Flare
import Flare.Smolder

socketHandler :: forall e.
                    (LogFiles -> Eff (chan :: Chan, console :: CONSOLE | e) Unit)
                 -> MessageEvent
                 -> Eff (chan :: Chan, console :: CONSOLE | e) Unit
socketHandler f event = do
  let received = runMessage (runMessageEvent event)
      eFiles = eitherDecode received :: Either String LogFiles
  case eFiles of
    Left msg -> log msg
    Right logFiles ->
        f logFiles

socketChannel url = do
  chan <- channel mempty
  Connection socket <- newWebSocket (URL url) []
  socket.onmessage $= socketHandler (send chan)

  return chan

addEventListener listener eType target =
    ET.addEventListener eType (ET.eventListener listener) true target

sendEvent chan event = send chan event

eventChannel def = do
    chan <- channel def
    return $ addEventListener (sendEvent chan)

logNav (LogFiles o) =
  buildVerticalNav $ NavBar
      { title: (Pills $ map (\(Tuple x idx) -> Pill x false idx) withIndex)
      , content: "Content goes here"
      }
 where
   withIndex = zip o.logFiles indices
   indices = 0 .. (length o.logFiles)


logFilesUI = lift (subscribe <$> socketChannel "ws://localhost:3000/log/Tmp")

ui = logNav <$> logFilesUI

main = do
  runFlareHTML "controls" "output" ui