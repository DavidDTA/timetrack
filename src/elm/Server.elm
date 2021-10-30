port module Server exposing (main)

import Json.Encode
import Platform

port requests: ((Json.Encode.Value, Json.Encode.Value) -> msg) -> Sub msg

port responses: (Json.Encode.Value, String) -> Cmd msg

type Msg =
  Request { request: Json.Encode.Value, response: Json.Encode.Value }

main: Platform.Program () () Msg
main =
    Platform.worker
        { init = always ((), Cmd.none)
        , update = update
        , subscriptions = always (requests (\(req, res) -> Request { request = req, response = res }))
        }

update msg model =
  case msg of
    Request { request, response } ->
      (model, responses (response, "Hello from Elm!"))
