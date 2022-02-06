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


port requests : (( Json.Encode.Value, Json.Encode.Value ) -> msg) -> Sub msg


port responses : ( Json.Encode.Value, Int, String ) -> Cmd msg


type alias Flags =
    { firebaseProjectId : String
    , firestoreHostPortOverride : Maybe ( String, Int )
    }


type alias Model =
    Maybe
        { firestore : Firestore.Firestore
        }


port errors : String -> Cmd msg


main =
    Functions.server
        { sharedInit = sharedInit
        , requestInit = requestInit
        , requestUpdate = requestUpdate
        , requestSubscriptions = requestSubscriptions
        , requestPort = requests
        , responsePort = responses
        , errorPort = errors
        , endpoint = Api.endpoint
        }


type RequestMsg
    = GetTimerSet (Result Firestore.Error TimerSet.TimerSet)


sharedInit flags =
    let
        result =
            Json.Decode.decodeValue
                (Json.Decode.map2 Flags
                    (Json.Decode.at [ "firebaseProjectId" ] Json.Decode.string)
                    (Json.Decode.at [ "firestoreHostPortOverride" ]
                        (Json.Decode.nullable Json.Decode.string
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
                )
                flags
    in
    case result of
        Ok { firebaseProjectId, firestoreHostPortOverride } ->
            ( Just
                { firestore =
                    Firestore.Config.new { apiKey = "", project = firebaseProjectId }
                        |> Maybe.Extra.unwrap identity (\( host, portInt ) -> Firestore.Config.withHost host portInt) firestoreHostPortOverride
                        |> Firestore.init
                }
            , Cmd.none
            )

        Err err ->
            ( Nothing
            , errors ("Failure to parse flags: " ++ Json.Decode.errorToString err)
            )


requestInit sharedModel { usernameByFiat, request } =
    case sharedModel of
        Nothing ->
            Functions.fail "server uninitialized"

        Just { firestore } ->
            case request of
                Api.Get ->
                    Functions.continue { cmd = Server.Storage.getTimerSet firestore usernameByFiat |> Task.attempt GetTimerSet, newRequestModel = () }

                Api.Update updates ->
                    Functions.continue { cmd = Server.Storage.updateTimerSet firestore usernameByFiat (\timerSet -> List.foldl Api.applyUpdate timerSet updates) |> Task.attempt GetTimerSet, newRequestModel = () }


requestUpdate msg model =
    case msg of
        GetTimerSet result ->
            case result of
                Ok timerSet ->
                    Functions.succeed (Api.Value timerSet)

                Err error ->
                    Functions.fail (firestoreErrorToString error)


requestSubscriptions model =
    Sub.none


firestoreErrorToString error =
    case error of
        Firestore.Http_ (Http.BadUrl url) ->
            "bad url: " ++ url

        Firestore.Http_ Http.Timeout ->
            "timeout"

        Firestore.Http_ Http.NetworkError ->
            "network error"

        Firestore.Http_ (Http.BadStatus status) ->
            "bad status: " ++ String.fromInt status

        Firestore.Http_ (Http.BadBody message) ->
            "bad body: " ++ message

        Firestore.Response { code, status, message } ->
            "(" ++ String.fromInt code ++ ") " ++ status ++ ": " ++ message
