module DebugStatus exposing (DebugStatus, fromUrl, hoppOverMeldingsanimasjon, meldingsTimeout)

import Url exposing (Url)


type DebugStatus
    = Debug
    | Regular



--debugParser : Parser DebugStatus
--debugParser =
--    succeed Debug
--        |. Parser.chompUntil "?"
--        |. Parser.chompUntil "debug=true"
--        |. Parser.symbol "&"


fromUrl : Url -> DebugStatus
fromUrl url =
    url
        |> Url.toString
        |> String.split "?"
        |> List.tail
        |> Maybe.andThen List.head
        |> Maybe.map (String.split "&")
        |> Maybe.map (List.member "debug=true")
        |> Maybe.map
            (\debug ->
                if debug then
                    Debug

                else
                    Regular
            )
        |> Maybe.withDefault Regular


meldingsTimeout : DebugStatus -> Float -> Float
meldingsTimeout status timeout =
    case status of
        Debug ->
            0

        Regular ->
            timeout


hoppOverMeldingsanimasjon : DebugStatus -> Bool
hoppOverMeldingsanimasjon debugStatus =
    debugStatus == Debug
