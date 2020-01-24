module DebugStatus exposing (DebugStatus, fromUrl, hoppOverMeldingsanimasjon, meldingsTimeout, regular, tickInterval)

import Url exposing (Url)


type DebugStatus
    = Debug
    | Regular


regular : DebugStatus
regular =
    Regular


tickInterval : DebugStatus -> Float
tickInterval debugStatus =
    case debugStatus of
        Debug ->
            -- mål tid hvert minutt
            1000 * 60

        Regular ->
            1000


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
