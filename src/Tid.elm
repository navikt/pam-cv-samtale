module Tid exposing (nyestePosix, nyesteSistLagretVerdi, tilKlokkeSlett, ulikDato)

import Time exposing (Posix, Zone)


nyestePosix : Posix -> Posix -> Posix
nyestePosix posix1 posix2 =
    if Time.posixToMillis posix1 > Time.posixToMillis posix2 then
        posix1

    else
        posix2


ulikDato : Zone -> Posix -> Posix -> Bool
ulikDato zone posix1 posix2 =
    if Time.toDay zone posix1 /= Time.toDay zone posix2 then
        True

    else if Time.toMonth zone posix1 /= Time.toMonth zone posix2 then
        True

    else if Time.toYear zone posix1 /= Time.toYear zone posix2 then
        True

    else
        False


tilKlokkeSlett : Zone -> Posix -> String
tilKlokkeSlett zone posix =
    let
        hour =
            posix
                |> Time.toHour zone
                |> String.fromInt
                |> String.padLeft 2 '0'

        minutes =
            posix
                |> Time.toMinute zone
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    hour ++ ":" ++ minutes


nyesteSistLagretVerdi : List Int -> Posix -> Posix
nyesteSistLagretVerdi sistLagretListe sistLagretFraForrigeSeksjon =
    case List.maximum sistLagretListe of
        Just value ->
            sistLagretFraForrigeSeksjon
                |> nyestePosix (Time.millisToPosix value)

        Nothing ->
            sistLagretFraForrigeSeksjon
