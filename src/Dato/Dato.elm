module Dato.Dato exposing
    ( Dato
    , DatoFeilmelding
    , DatoPeriode(..)
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
    , formaterDag
    , getDatoDag
    , getDatoMåned
    , getDatoÅr
    , periodeTilString
    , stringTilÅr
    , toString
    , validerDato
    , årTilString
    )

import Dato.Maned as Måned exposing (Måned(..))
import Json.Decode exposing (Decoder)
import Json.Encode


type alias DatoInfo =
    { dag : String
    , måned : Måned
    , år : År
    }


type Dato
    = Dato DatoInfo


toString : Dato -> String
toString dato =
    case dato of
        Dato dato_ ->
            dato_.dag ++ "-" ++ Måned.tilNummer dato_.måned ++ "-" ++ årTilString dato_.år


getDatoÅr : Dato -> String
getDatoÅr (Dato datoInfo) =
    årTilString datoInfo.år


getDatoMåned : Dato -> Måned
getDatoMåned (Dato info) =
    info.måned


getDatoDag : Dato -> String
getDatoDag (Dato info) =
    info.dag


type DatoValidering
    = DatoValiderer Dato
    | DatoValideringsfeil
    | DatoIkkeSkrevetInn


validerDato : { dag : String, måned : Maybe Måned, år : String } -> DatoValidering
validerDato { dag, måned, år } =
    case måned of
        Just måned_ ->
            case ( stringTilÅr år, validerDag dag ) of
                ( Just år_, True ) ->
                    DatoValiderer
                        (Dato
                            { dag = formaterDag dag
                            , måned = måned_
                            , år = år_
                            }
                        )

                _ ->
                    DatoValideringsfeil

        Nothing ->
            if String.isEmpty dag && String.isEmpty år then
                DatoIkkeSkrevetInn

            else
                DatoValideringsfeil


type alias DatoFeilmelding =
    { feilmelding : String
    , feilPåDag : Bool
    , feilPåMåned : Bool
    , feilPåÅr : Bool
    }


feilmeldingForDato : { dag : String, måned : Maybe Måned, år : String } -> Maybe DatoFeilmelding
feilmeldingForDato { dag, måned, år } =
    case måned of
        Just måned_ ->
            if stringTilÅr år == Nothing then
                Just
                    { feilmelding = "År kan kun ha fire siffer"
                    , feilPåDag = False
                    , feilPåMåned = False
                    , feilPåÅr = True
                    }

            else if not (validerDag dag) then
                Just
                    { feilmelding = "Dag må være et tall mellom 1 og 31"
                    , feilPåDag = True
                    , feilPåMåned = False
                    , feilPåÅr = False
                    }

            else
                Nothing

        Nothing ->
            if String.isEmpty dag && String.isEmpty år then
                Nothing

            else
                Just
                    { feilmelding = "Du må velge måned"
                    , feilPåDag = False
                    , feilPåMåned = True
                    , feilPåÅr = False
                    }


formaterDag : String -> String
formaterDag dag =
    if String.length dag == 1 then
        "0" ++ dag

    else
        dag


validerDag : String -> Bool
validerDag dag =
    case String.toInt dag of
        Just dagSomInt ->
            dagSomInt > 0 && dagSomInt <= 31

        Nothing ->
            False


encodeMaybeDato : Maybe Dato -> Json.Encode.Value
encodeMaybeDato dato =
    case dato of
        Just (Dato dato_) ->
            Json.Encode.string (årTilString dato_.år ++ "-" ++ Måned.tilNummer dato_.måned ++ "-" ++ dato_.dag)

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
