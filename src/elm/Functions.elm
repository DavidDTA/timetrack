module Functions exposing (continue, fail, server, succeed)

import Dict
import IncrementId
import Json.Decode
import Json.Encode
import Serialize


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
    = Fail
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


server { sharedInit, requestInit, requestUpdate, requestSubscriptions, requestPort, responsePort, parseRequest, serializeResponse } =
    Platform.worker
        { init = init sharedInit
        , update = update requestInit requestUpdate responsePort parseRequest serializeResponse
        , subscriptions = subscriptions requestPort requestSubscriptions
        }


init sharedInit flags =
    ( { requests = Dict.empty
      , sharedModel = sharedInit flags
      , nextId = IncrementId.zero
      }
    , Cmd.none
    )


updateForStep responsePort serializeResponse id responseToken result model =
    case result of
        Fail ->
            ( { model | requests = Dict.remove id model.requests }
            , responsePort ( responseToken, 500, "" )
            )

        Continue { newRequestModel, cmd } ->
            ( { model | requests = Dict.insert id newRequestModel model.requests }
            , Cmd.map (\requestMsg -> Continuation { id = id, requestMsg = requestMsg }) cmd
            )

        Succeed response ->
            ( { model | requests = Dict.remove id model.requests }
            , responsePort ( responseToken, 200, serializeResponse response )
            )


update requestInit requestUpdate responsePort parseRequest serializeResponse msg model =
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
                                    case parseRequest body of
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
                    updateForStep responsePort serializeResponse model.nextId responseToken (requestInit parsedRequest) { model | nextId = IncrementId.increment model.nextId }

        Continuation { id, requestMsg } ->
            case Dict.get id model.requests of
                Nothing ->
                    ( model, Cmd.none )

                Just { responseToken, requestModel } ->
                    updateForStep responsePort serializeResponse id responseToken (requestUpdate requestMsg requestModel) model


subscriptions requestPort requestSubscriptions model =
    Sub.batch
        [ requestPort (\( request, responseToken ) -> NewRequest { request = request, responseToken = responseToken })
        , Dict.toList model.requests
            |> List.map (\( id, requestModel ) -> Sub.map (\requestMsg -> Continuation { id = id, requestMsg = requestMsg }) (requestSubscriptions requestModel))
            |> Sub.batch
        ]
