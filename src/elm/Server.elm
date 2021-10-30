port module Server exposing (main)

import Json.Encode
import Platform


port requests : (( Json.Encode.Value, Json.Encode.Value ) -> msg) -> Sub msg


port responses : ( Json.Encode.Value, String ) -> Cmd msg


type alias Model =
    { authToken : Maybe String
    }


type Msg
    = Request { request : Json.Encode.Value, response : Json.Encode.Value }


error _ =
    Cmd.none


main : Platform.Program () () Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always (requests (\( req, res ) -> Request { request = req, response = res }))
        }


init flags =
    let
        result =
            Json.Decode.decodeValue
                (Json.Decode.at [ "auth", "token" ] Json.Decode.String)
                flags
    in
    case result of
        Ok value ->
            ( { authToken = Just value }, Cmd.none )

        Error err ->
            ( { authToken = Nothing }, error err )


update msg model =
    case msg of
        Request { request, response } ->
            ( model, responses ( response, "Hello from Elm!" ) )
