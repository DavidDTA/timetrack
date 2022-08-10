port module Server exposing (main)

import Api
import Firestore
import Firestore.Config
import Functions
import Http
import Json.Decode
import Json.Encode
import Maybe.Extra
import Process
import Server.Storage
import Task
import Timeline
import TimerSet
import Version


port requests : (( Json.Encode.Value, Json.Encode.Value ) -> msg) -> Sub msg


port responses : ( Json.Encode.Value, Int, String ) -> Cmd msg


port errors : String -> Cmd msg


type alias Flags =
    { firebaseProjectId : String
    , firestoreHostPortOverride : Maybe ( String, Int )
    }


type alias Model =
    { firestore : Maybe Firestore.Firestore
    , firestoreQueuedRequests : List ( Json.Encode.Value, Json.Encode.Value )
    }


type Msg
    = NewRequest ( Json.Encode.Value, Json.Encode.Value )
    | GotTimerSet Json.Encode.Value (Result Server.Storage.Error { version : Version.Version, value : TimerSet.TimerSet })
    | FunctionsAccessTokenElapsed String
    | FunctionsAccessTokenReceived String (Result Http.Error Functions.Credential)


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init flags =
    let
        result =
            Json.Decode.decodeValue
                (Json.Decode.map2 Flags
                    (Json.Decode.at [ "GCLOUD_PROJECT" ] Json.Decode.string)
                    (Json.Decode.maybe (Json.Decode.at [ "FIRESTORE_EMULATOR_HOST" ] Json.Decode.string)
                        |> Json.Decode.andThen
                            (\hostPortStringMaybe ->
                                case hostPortStringMaybe of
                                    Nothing ->
                                        Json.Decode.succeed Nothing

                                    Just hostPortString ->
                                        case String.split ":" hostPortString of
                                            host :: portString :: [] ->
                                                case String.toInt portString of
                                                    Nothing ->
                                                        Json.Decode.fail ""

                                                    Just portInt ->
                                                        Json.Decode.succeed (Just ( "http://" ++ host, portInt ))

                                            _ ->
                                                Json.Decode.fail ""
                            )
                    )
                )
                flags
    in
    case result of
        Ok { firebaseProjectId, firestoreHostPortOverride } ->
            ( { firestore =
                    case firestoreHostPortOverride of
                        Nothing ->
                            Nothing

                        Just ( host, portInt ) ->
                            Firestore.Config.new { apiKey = "", project = firebaseProjectId }
                                |> Firestore.Config.withHost host portInt
                                |> Firestore.init
                                |> Just
              , firestoreQueuedRequests = []
              }
            , if Maybe.Extra.isNothing firestoreHostPortOverride then
                Functions.getAccessToken (FunctionsAccessTokenReceived firebaseProjectId)

              else
                Cmd.none
            )

        Err err ->
            ( { firestore = Nothing
              , firestoreQueuedRequests = []
              }
            , errors ("Failure to parse flags: " ++ Json.Decode.errorToString err)
            )


serverError responseValue errorMessage =
    Cmd.batch
        [ responses ( responseValue, 500, "" )
        , errors ("Server error: " ++ errorMessage)
        ]


enqueueRequestForFirestore requestValue responseValue model =
    ( { model
        | firestoreQueuedRequests = ( requestValue, responseValue ) :: model.firestoreQueuedRequests
      }
    , Cmd.none
    )


update msg model =
    case msg of
        NewRequest ( requestValue, responseValue ) ->
            case Functions.receive Api.endpoint { request = requestValue, response = responseValue } of
                Result.Err err ->
                    ( model, responses ( responseValue, 400, "" ) )

                Result.Ok { usernameByFiat, request } ->
                    case request of
                        Api.Get ->
                            case model.firestore of
                                Just firestore ->
                                    ( model
                                    , Server.Storage.getTimerSet firestore usernameByFiat
                                        |> Task.attempt (GotTimerSet responseValue)
                                    )

                                Nothing ->
                                    enqueueRequestForFirestore requestValue responseValue model

                        Api.Update preconditionVersion updates ->
                            case model.firestore of
                                Just firestore ->
                                    ( model
                                    , Server.Storage.updateTimerSet firestore usernameByFiat preconditionVersion (\timerSet -> List.foldl Api.applyUpdate timerSet updates)
                                        |> Task.attempt (GotTimerSet responseValue)
                                    )

                                Nothing ->
                                    enqueueRequestForFirestore requestValue responseValue model

        GotTimerSet responseValue result ->
            case result of
                Ok value ->
                    ( model, responses ( responseValue, 200, Functions.respond Api.endpoint (Api.Value value) ) )

                Err error ->
                    ( model, serverError responseValue (storageErrorToString error) )

        FunctionsAccessTokenElapsed firebaseProjectId ->
            ( model
            , Functions.getAccessToken (FunctionsAccessTokenReceived firebaseProjectId)
            )

        FunctionsAccessTokenReceived firebaseProjectId result ->
            case result of
                Result.Err err ->
                    ( model, errors "Functions credential error" )

                Result.Ok credential ->
                    model.firestoreQueuedRequests
                        |> List.foldr
                            (\req ( accModel, accCmd ) ->
                                update (NewRequest req) accModel
                                    |> Tuple.mapSecond (\newCmd -> Cmd.batch [ newCmd, accCmd ])
                            )
                            ( { model
                                | firestore =
                                    Firestore.Config.new { apiKey = "", project = firebaseProjectId }
                                        |> Firestore.Config.withAuthorization credential.accessToken
                                        |> Firestore.init
                                        |> Just
                              }
                            , Process.sleep (toFloat credential.expiresInSeconds * 1000 / 3)
                                |> Task.perform (always (FunctionsAccessTokenElapsed firebaseProjectId))
                            )


subscriptions model =
    requests NewRequest


httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "bad url: " ++ url

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody message ->
            "bad body: " ++ message


firestoreErrorToString error =
    case error of
        Firestore.Path_ (Firestore.InvalidPath invalidPath) ->
            "invalid path: " ++ invalidPath

        Firestore.Http_ httpError ->
            httpErrorToString httpError

        Firestore.Response { code, status, message } ->
            "(" ++ String.fromInt code ++ ") " ++ status ++ ": " ++ message


storageErrorToString error =
    case error of
        Server.Storage.PreconditionFailure ->
            "precondition failure"

        Server.Storage.FirestoreError firestoreError ->
            firestoreErrorToString firestoreError
