module Feilmelding exposing (Feilmelding, encode, feilmelding)

import Http
import Json.Encode


type Feilmelding
    = Feilmelding
        { operasjon : String
        , errorType : String
        , message : Maybe String
        }


feilmelding : String -> Http.Error -> Maybe Feilmelding
feilmelding operasjon error =
    case error of
        Http.BadUrl message ->
            Feilmelding
                { operasjon = operasjon
                , errorType = "BadUrl"
                , message = Just message
                }
                |> Just

        Http.Timeout ->
            Feilmelding
                { operasjon = operasjon
                , errorType = "Timeout"
                , message = Nothing
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
                }
                |> Just

        Http.BadBody message ->
            Feilmelding
                { operasjon = operasjon
                , errorType = "BadBody"
                , message = Just message
                }
                |> Just


encode : Feilmelding -> Json.Encode.Value
encode (Feilmelding info) =
    Json.Encode.object
        [ ( "operasjon", Json.Encode.string info.operasjon )
        , ( "errorType", Json.Encode.string info.errorType )
        , ( "message"
          , info.message
                |> Maybe.map Json.Encode.string
                |> Maybe.withDefault Json.Encode.null
          )
        ]
