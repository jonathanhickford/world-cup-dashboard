port module Ports exposing (..)


port refreshPage : () -> Cmd msg


port newServiceWorkerAvailable : (Bool -> msg) -> Sub msg
