module Functions exposing (Endpoint, Error(..), codecEndpoint, continue, fail, send, server, succeed)

import Dict
import Http
import IncrementId
import Json.Decode
import Json.Encode
import Serialize


type Endpoint req res
    = Endpoint
        { path : String
        , requestCodec : Serialize.Codec Never req
        , responseCodec : Serialize.Codec Never res
        }


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | HttpError Http.Metadata String
    | MalformedJson Json.Decode.Error
    | SerializationError (Serialize.Error Never)


codecEndpoint path requestCodec responseCodec =
    Endpoint { path = path, requestCodec = requestCodec, responseCodec = responseCodec }


send : Endpoint req res -> req -> (Result Error res -> msg) -> Cmd msg
send (Endpoint { path, requestCodec, responseCodec }) request tag =
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


type alias Model sharedModel requestModel =
    { requests :
        Dict.Dict
            IncrementId.Id
            { responseToken : Json.Encode.Value
            , requestModel : requestModel
            }
    , nextId : IncrementId.Id
    , sharedModel : sharedModel
    }


type Msg requestMsg
    = NewRequest
        { request : Json.Encode.Value
        , responseToken : Json.Encode.Value
        }
    | Continuation
        { id : IncrementId.Id
        , requestMsg : requestMsg
        }


type Step response requestModel requestMsg
    = Fail String
    | Succeed response
    | Continue
        { newRequestModel : requestModel
        , cmd : Cmd requestMsg
        }


fail =
    Fail


succeed =
    Succeed


continue =
    Continue


server ({ endpoint } as config) =
    Platform.worker
        { init = init config
        , update = update config endpoint
        , subscriptions = subscriptions config
        }


init { sharedInit } flags =
    let
        ( sharedModel, cmd ) =
            sharedInit flags
    in
    ( { requests = Dict.empty
      , sharedModel = sharedModel
      , nextId = IncrementId.zero
      }
    , cmd
    )


updateForStep { responsePort, errorPort } responseCodec id responseToken result model =
    case result of
        Fail message ->
            ( { model | requests = Dict.remove id model.requests }
            , Cmd.batch
                [ responsePort ( responseToken, 500, "" )
                , errorPort message
                ]
            )

        Continue { newRequestModel, cmd } ->
            ( { model | requests = Dict.insert id { responseToken = responseToken, requestModel = newRequestModel } model.requests }
            , Cmd.map (\requestMsg -> Continuation { id = id, requestMsg = requestMsg }) cmd
            )

        Succeed response ->
            ( { model | requests = Dict.remove id model.requests }
            , responsePort ( responseToken, 200, Serialize.encodeToJson responseCodec response |> Json.Encode.encode 0 )
            )


update ({ requestInit, requestUpdate, responsePort, errorPort } as config) (Endpoint { requestCodec, responseCodec }) msg model =
    case msg of
        NewRequest { request, responseToken } ->
            let
                decodeRequest =
                    Json.Decode.map2
                        Tuple.pair
                        (Json.Decode.field "method" Json.Decode.string)
                        (Json.Decode.field "body" Json.Decode.value)
                        |> Json.Decode.andThen
                            (\( method, body ) ->
                                if method == "POST" then
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
                            )
            in
            case Json.Decode.decodeValue decodeRequest request of
                Result.Err _ ->
                    ( model, responsePort ( responseToken, 400, "" ) )

                Result.Ok parsedRequest ->
                    updateForStep config responseCodec model.nextId responseToken (requestInit model.sharedModel parsedRequest) { model | nextId = IncrementId.increment model.nextId }

        Continuation { id, requestMsg } ->
            case Dict.get id model.requests of
                Nothing ->
                    ( model, errorPort "unexpected message" )

                Just { responseToken, requestModel } ->
                    updateForStep config responseCodec id responseToken (requestUpdate requestMsg requestModel) model


subscriptions { requestPort, requestSubscriptions } model =
    Sub.batch
        [ requestPort (\( request, responseToken ) -> NewRequest { request = request, responseToken = responseToken })
        , Dict.toList model.requests
            |> List.map (\( id, requestModel ) -> Sub.map (\requestMsg -> Continuation { id = id, requestMsg = requestMsg }) (requestSubscriptions requestModel))
            |> Sub.batch
        ]
