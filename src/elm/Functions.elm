module Functions exposing (Credential, Endpoint, SendError(..), endpoint, getAccessToken, receive, respond, send)

import Dict
import Http
import IncrementId
import Json.Decode
import Json.Encode
import Serialize


type alias Credential =
    { accessToken : String, expiresInSeconds : Int }


{-| Retrieves an access token according to ComputeEngineCredential. See: <https://github.com/firebase/firebase-admin-node/blob/master/src/app/credential-internal.ts#L195>
-}
getAccessToken toMsg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Metadata-Flavor" "Google" ]
        , url = "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect =
            Http.expectJson toMsg
                (Json.Decode.map2 Credential
                    (Json.Decode.at [ "access_token" ] Json.Decode.string)
                    (Json.Decode.at [ "expires_in" ] Json.Decode.int)
                )
        }


type Endpoint req res
    = Endpoint
        { path : String
        , requestCodec : Serialize.Codec Never req
        , responseCodec : Serialize.Codec Never res
        }


type SendError
    = BadUrl String
    | Timeout
    | NetworkError
    | HttpError Http.Metadata String
    | MalformedJson Json.Decode.Error
    | SerializationError (Serialize.Error Never)


type ReceiveError
    = ReceiveError


endpoint path requestCodec responseCodec =
    Endpoint { path = path, requestCodec = requestCodec, responseCodec = responseCodec }


send : Endpoint req res -> (Result SendError res -> msg) -> req -> Cmd msg
send (Endpoint { path, requestCodec, responseCodec }) tag request =
    Http.request
        { method = "POST"
        , headers = []
        , url = path
        , body = Http.jsonBody (Serialize.encodeToJson requestCodec request)
        , expect =
            Http.expectStringResponse tag
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            Result.Err (BadUrl url)

                        Http.Timeout_ ->
                            Result.Err Timeout

                        Http.NetworkError_ ->
                            Result.Err NetworkError

                        Http.BadStatus_ metadata body ->
                            Result.Err (HttpError metadata body)

                        Http.GoodStatus_ metadata body ->
                            Json.Decode.decodeString Json.Decode.value body
                                |> Result.mapError MalformedJson
                                |> Result.andThen (Serialize.decodeFromJson responseCodec >> Result.mapError SerializationError)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


receive : Endpoint req res -> { request : Json.Decode.Value, response : Json.Encode.Value } -> Result ReceiveError req
receive (Endpoint { path, requestCodec }) { request, response } =
    let
        decodeRequest =
            Json.Decode.map3
                (\method requestPath body -> { method = method, requestPath = requestPath, body = body })
                (Json.Decode.field "method" Json.Decode.string)
                (Json.Decode.field "path" Json.Decode.string)
                (Json.Decode.field "body" Json.Decode.value)
                |> Json.Decode.andThen
                    (\{ method, requestPath, body } ->
                        if method == "POST" then
                            if requestPath == path then
                                case Serialize.decodeFromJson requestCodec body of
                                    Result.Err err ->
                                        Json.Decode.fail
                                            (case err of
                                                Serialize.CustomError _ ->
                                                    ""

                                                Serialize.DataCorrupted ->
                                                    ""

                                                Serialize.SerializerOutOfDate ->
                                                    ""
                                            )

                                    Result.Ok parsed ->
                                        Json.Decode.succeed parsed

                            else
                                Json.Decode.fail ""

                        else
                            Json.Decode.fail ""
                    )
    in
    Json.Decode.decodeValue decodeRequest request
        |> Result.mapError (always ReceiveError)


respond : Endpoint req res -> res -> String
respond (Endpoint { path, responseCodec }) response =
    Serialize.encodeToJson responseCodec response
        |> Json.Encode.encode 0
