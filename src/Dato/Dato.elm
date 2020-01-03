module Dato.Dato exposing
    ( DatoPeriode(..)
    , DatoValidering(..)
    , TilDato(..)
    , År
    , datoTilString
    , decodeMonthYear
    , encodeMaybeDato
    , encodeMonthYear
    , feilmeldingForDato
    , feilmeldingValgfriMåned
    , feilmeldingÅr
    , justerDatoFormat
    , periodeTilString
    , stringTilÅr
    , toValidertDatoFormat
    , validerDato
    , årTilString
    )

import Dato.Maned as Måned exposing (Måned(..))
import Json.Decode exposing (Decoder)
import Json.Encode


toValidertDatoFormat : String -> String
toValidertDatoFormat dato =
    String.split "." dato
        |> List.reverse
        |> String.join "-"


type DatoValidering
    = GyldigDato String
    | DatoValideringsfeil String
    | DatoIkkeSkrevetInn


feilmeldingForDato : String -> Maybe String
feilmeldingForDato dato =
    case validerDato dato of
        DatoValideringsfeil feilmelding ->
            Just feilmelding

        _ ->
            Nothing


validerDato : String -> DatoValidering
validerDato dato =
    let
        elementList =
            String.replace "," "." dato
                |> String.split "."
    in
    if String.isEmpty dato then
        DatoIkkeSkrevetInn

    else if List.length elementList /= 3 then
        DatoValideringsfeil "Dato må skrives dd.mm.åååå"

    else
        case elementList of
            dag :: måned :: år :: [] ->
                if not (dagErGyldig dag) then
                    DatoValideringsfeil "Dag må være et tall mellom 1–31"

                else if not (månedErGyldig måned) then
                    DatoValideringsfeil "Måned må være et tall mellom 1–12"

                else if stringTilÅr år == Nothing then
                    DatoValideringsfeil "År må være 4 siffer"

                else
                    GyldigDato dato

            _ ->
                DatoValideringsfeil "Dato må skrives dd.mm.åååå"


justerDatoFormat : String -> String
justerDatoFormat input =
    case validerDato input of
        GyldigDato _ ->
            -- bytt ut komma med punktum, padd med 0 foran dag og måned hvis kun et tall.
            let
                elementList =
                    String.trim input
                        |> String.replace "," "."
                        |> String.split "."
            in
            case elementList of
                dag :: måned :: år :: [] ->
                    String.padLeft 2 '0' dag ++ "." ++ String.padLeft 2 '0' måned ++ "." ++ år

                _ ->
                    input

        _ ->
            input


dagErGyldig : String -> Bool
dagErGyldig dag =
    case (String.trim >> String.toInt) dag of
        Just dagSomInt ->
            dagSomInt > 0 && dagSomInt <= 31

        Nothing ->
            False


månedErGyldig : String -> Bool
månedErGyldig måned =
    case (String.trim >> String.toInt) måned of
        Just månedSomInt ->
            månedSomInt > 0 && månedSomInt <= 12

        Nothing ->
            False


encodeMaybeDato : Maybe String -> Json.Encode.Value
encodeMaybeDato dato =
    case dato of
        Just dato_ ->
            Json.Encode.string (toValidertDatoFormat dato_)

        Nothing ->
            Json.Encode.null



--- ÅR ---


type År
    = År String


årTilString : År -> String
årTilString (År år_) =
    år_


stringTilÅr : String -> Maybe År
stringTilÅr string =
    case feilmeldingÅr string of
        Just _ ->
            Nothing

        Nothing ->
            Just (År (String.trim string))


feilmeldingÅr : String -> Maybe String
feilmeldingÅr år_ =
    let
        trimmetÅr =
            String.trim år_
    in
    if String.isEmpty trimmetÅr then
        Just "Skriv årstall"

    else if String.length trimmetÅr /= 4 || String.toInt trimmetÅr == Nothing then
        Just "Kun 4 siffer"

    else
        Nothing


decodeMonthYear : String -> Decoder ( Måned, År )
decodeMonthYear string =
    case String.split "-" string of
        årString :: månedString :: [] ->
            case ( stringTilÅr årString, Måned.fraNummer månedString ) of
                ( Just år_, Just måned_ ) ->
                    Json.Decode.succeed ( måned_, år_ )

                _ ->
                    Json.Decode.fail ("Kan ikke decode YearMonth \"" ++ string ++ "\". Forventet streng på formen \"yyyy-mm\"")

        _ ->
            Json.Decode.fail ("Kan ikke decode YearMonth \"" ++ string ++ "\". Forventet streng på formen \"yyyy-mm\"")


encodeMonthYear : Måned -> År -> Json.Encode.Value
encodeMonthYear måned_ (År år_) =
    Json.Encode.string (år_ ++ "-" ++ Måned.tilNummer måned_)



--- TilDato ---


type TilDato
    = Nåværende
    | Avsluttet Måned År


periodeTilString : Måned -> År -> TilDato -> String
periodeTilString måned_ år_ tilDato =
    datoTilString måned_ år_
        ++ " - "
        ++ (tilDatoTilString >> String.toLower) tilDato


tilDatoTilString : TilDato -> String
tilDatoTilString tilDato =
    case tilDato of
        Avsluttet måned_ år_ ->
            datoTilString måned_ år_

        Nåværende ->
            "nåværende"


datoTilString : Måned -> År -> String
datoTilString måned_ år_ =
    Måned.tilString måned_ ++ " " ++ årTilString år_



--- Valgfri Periode ---


type DatoPeriode
    = IkkeOppgitt
    | Oppgitt Måned År TilDato


feilmeldingValgfriMåned : Maybe Måned -> Maybe String
feilmeldingValgfriMåned måned_ =
    if måned_ == Nothing then
        Just "Velg måned"

    else
        Nothing
