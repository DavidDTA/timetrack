port module Server exposing (main)

import Api
import Functions
import Json.Decode
import Json.Encode


port requests : (( Json.Encode.Value, Json.Encode.Value ) -> msg) -> Sub msg


port responses : ( Json.Encode.Value, Int, String ) -> Cmd msg


type alias Model =
    { authToken : String
    }


error _ =
    Cmd.none


main =
    Functions.server
        { sharedInit = sharedInit
        , requestInit = requestInit
        , requestUpdate = requestUpdate
        , requestSubscriptions = requestSubscriptions
        , requestPort = requests
        , responsePort = responses
        , parseRequest = Api.receive
        , serializeResponse = Api.respond
        }


sharedInit flags =
    let
        result =
            Json.Decode.decodeValue
                (Json.Decode.at [ "auth", "token" ] Json.Decode.string)
                flags
    in
    case result of
        Ok value ->
            ( { authToken = Just value }, Cmd.none )

        Err err ->
            ( { authToken = Nothing }, error err )


requestInit request =
    Functions.fail


requestUpdate msg model =
    Functions.fail


requestSubscriptions model =
    Sub.none
