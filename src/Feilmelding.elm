module Feilmelding exposing (Feilmelding, encode, feilmelding, withRequestBody)

import Http
import Json.Encode


type Feilmelding
    = Feilmelding
        { operasjon : String
        , errorType : String
        , message : Maybe String
        , requestBody : Maybe Json.Encode.Value
        }


feilmelding : String -> Http.Error -> Maybe Feilmelding
feilmelding operasjon error =
    case error of
        Http.BadUrl message ->
            Feilmelding
                { operasjon = operasjon
                , errorType = "BadUrl"
                , message = Just message
                , requestBody = Nothing
                }
                |> Just

        Http.Timeout ->
            Feilmelding
                { operasjon = operasjon
                , errorType = "Timeout"
                , message = Nothing
                , requestBody = Nothing
                }
                |> Just

        Http.NetworkError ->
            Nothing

        Http.BadStatus statusCode ->
            Feilmelding
                { operasjon = operasjon
                , errorType = "BadStatus"
                , message =
                    statusCode
                        |> String.fromInt
                        |> Just
                , requestBody = Nothing
                }
                |> Just

        Http.BadBody message ->
            Feilmelding
                { operasjon = operasjon
                , errorType = "BadBody"
                , message = Just message
                , requestBody = Nothing
                }
                |> Just


withRequestBody : Json.Encode.Value -> Feilmelding -> Feilmelding
withRequestBody requestBody (Feilmelding info) =
    Feilmelding { info | requestBody = Just requestBody }


encode : Feilmelding -> Json.Encode.Value
encode (Feilmelding info) =
    Json.Encode.object
        [ ( "operasjon", Json.Encode.string info.operasjon )
        , ( "errorType", Json.Encode.string info.errorType )
        , ( "requestBody", Maybe.withDefault Json.Encode.null info.requestBody )
        , ( "message"
          , Feilmelding info
                |> beskrivelse
                |> Json.Encode.string
          )
        ]


beskrivelse : Feilmelding -> String
beskrivelse (Feilmelding info) =
    case info.message of
        Just message ->
            info.errorType ++ " " ++ message ++ " på operasjon \"" ++ info.operasjon ++ "\""

        Nothing ->
            info.errorType ++ " på operasjon \"" ++ info.operasjon ++ "\""
