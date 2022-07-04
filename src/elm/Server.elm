port module Server exposing (main)

import Api
import Firestore
import Firestore.Config
import Functions
import Http
import Json.Decode
import Json.Encode
import Maybe.Extra
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
    }


type Msg
    = NewRequest ( Json.Encode.Value, Json.Encode.Value )
    | GotTimerSet Json.Encode.Value (Result Firestore.Error { version : Version.Version, value : TimerSet.TimerSet })
    | FunctionsReceivedAccessToken String (Result Http.Error Functions.Credential)


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
              }
            , if Maybe.Extra.isNothing firestoreHostPortOverride then
                Functions.getAccessToken (FunctionsReceivedAccessToken firebaseProjectId)

              else
                Cmd.none
            )

        Err err ->
            ( { firestore = Nothing }
            , errors ("Failure to parse flags: " ++ Json.Decode.errorToString err)
            )


serverError responseValue errorMessage =
    Cmd.batch
        [ responses ( responseValue, 500, "" )
        , errors ("Server error: " ++ errorMessage)
        ]


update msg model =
    case msg of
        NewRequest ( requestValue, responseValue ) ->
            case model.firestore of
                Just firestore ->
                    case Functions.receive Api.endpoint { request = requestValue, response = responseValue } of
                        Result.Err err ->
                            ( model, responses ( responseValue, 400, "" ) )

                        Result.Ok { usernameByFiat, request } ->
                            case request of
                                Api.Get ->
                                    ( model
                                    , Server.Storage.getTimerSet firestore usernameByFiat
                                        |> Task.attempt (GotTimerSet responseValue)
                                    )

                                Api.Update version updates ->
                                    ( model
                                    , Server.Storage.updateTimerSet firestore usernameByFiat (\timerSet -> List.foldl Api.applyUpdate timerSet updates)
                                        |> Task.attempt (GotTimerSet responseValue)
                                    )

                Nothing ->
                    ( model, serverError responseValue "server uninitialized" )

        GotTimerSet responseValue result ->
            case result of
                Ok value ->
                    ( model, responses ( responseValue, 200, Functions.respond Api.endpoint (Api.Value value) ) )

                Err error ->
                    ( model, serverError responseValue (firestoreErrorToString error) )

        FunctionsReceivedAccessToken firebaseProjectId result ->
            case result of
                Result.Err err ->
                    ( model, errors "Functions credential error" )

                Result.Ok credential ->
                    ( { model
                        | firestore =
                            Firestore.Config.new { apiKey = "", project = firebaseProjectId }
                                |> Firestore.Config.withAuthorization credential.accessToken
                                |> Firestore.init
                                |> Just
                      }
                    , Cmd.none
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
        Firestore.Http_ httpError ->
            httpErrorToString httpError

        Firestore.Response { code, status, message } ->
            "(" ++ String.fromInt code ++ ") " ++ status ++ ": " ++ message
