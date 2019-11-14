module Dato exposing
    ( Dato
    , DatoValidering(..)
    , Måned(..)
    , TilDato(..)
    , År
    , datoTilString
    , decodeMonthYear
    , encodeMaybeDato
    , encodeMonthYear
    , feilmeldingÅr
    , formaterDag
    , getDatoDag
    , getDatoMåned
    , getDatoÅr
    , månedTilNummerMåned
    , månedTilString
    , periodeTilString
    , stringTilMaybeMåned
    , stringTilMåned
    , stringTilÅr
    , toString
    , validerDato
    , validerÅr
    , årTilString
    )

import Json.Decode exposing (Decoder)
import Json.Encode


type Måned
    = Januar
    | Februar
    | Mars
    | April
    | Mai
    | Juni
    | Juli
    | August
    | September
    | Oktober
    | November
    | Desember
    | Ikke_valgt


type alias DatoInfo =
    { dag : String
    , måned : Måned
    , år : String
    }


type Dato
    = Dato DatoInfo


toString : Dato -> String
toString dato =
    case dato of
        Dato dato_ ->
            dato_.dag ++ "-" ++ månedTilNummerMåned dato_.måned ++ "-" ++ dato_.år


maybeDatoToString : Maybe Dato -> Maybe String
maybeDatoToString dato =
    case dato of
        Just (Dato info) ->
            Just ("" ++ info.år)

        Nothing ->
            Nothing


getDatoÅr : Dato -> String
getDatoÅr (Dato datoInfo) =
    datoInfo.år


getDatoMåned : Dato -> Måned
getDatoMåned (Dato info) =
    info.måned


getDatoDag : Dato -> String
getDatoDag (Dato info) =
    info.dag


type DatoValidering
    = DatoValiderer Dato
    | DatoValidererIkke
    | DatoIkkeSkrevetInn


validerDato : { dag : String, måned : Maybe Måned, år : String } -> DatoValidering
validerDato { dag, måned, år } =
    case måned of
        Just måned_ ->
            if validerÅr år && validerDag dag then
                DatoValiderer
                    (Dato
                        { dag = formaterDag dag
                        , måned = måned_
                        , år = år
                        }
                    )

            else
                DatoValidererIkke

        Nothing ->
            if String.isEmpty dag && String.isEmpty år then
                DatoIkkeSkrevetInn

            else
                DatoValidererIkke


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
            Json.Encode.string (dato_.år ++ "-" ++ månedTilNummerMåned dato_.måned ++ "-" ++ dato_.dag)

        Nothing ->
            Json.Encode.null


månedTilString : Måned -> String
månedTilString mnd =
    case mnd of
        Januar ->
            "Januar"

        Februar ->
            "Februar"

        Mars ->
            "Mars"

        April ->
            "April"

        Mai ->
            "Mai"

        Juni ->
            "Juni"

        Juli ->
            "Juli"

        August ->
            "August"

        September ->
            "September"

        Oktober ->
            "Oktober"

        November ->
            "November"

        Desember ->
            "Desember"

        Ikke_valgt ->
            ""


stringTilMåned : String -> Måned
stringTilMåned string =
    case string of
        "Januar" ->
            Januar

        "Februar" ->
            Februar

        "Mars" ->
            Mars

        "April" ->
            April

        "Mai" ->
            Mai

        "Juni" ->
            Juni

        "Juli" ->
            Juli

        "August" ->
            August

        "September" ->
            September

        "Oktober" ->
            Oktober

        "November" ->
            November

        "Desember" ->
            Desember

        "01" ->
            Januar

        "02" ->
            Februar

        "03" ->
            Mars

        "04" ->
            April

        "05" ->
            Mai

        "06" ->
            Juni

        "07" ->
            Juli

        "08" ->
            August

        "09" ->
            September

        "10" ->
            Oktober

        "11" ->
            November

        "12" ->
            Desember

        _ ->
            Januar


stringTilMaybeMåned : String -> Maybe Måned
stringTilMaybeMåned string =
    case string of
        "Januar" ->
            Just Januar

        "Februar" ->
            Just Februar

        "Mars" ->
            Just Mars

        "April" ->
            Just April

        "Mai" ->
            Just Mai

        "Juni" ->
            Just Juni

        "Juli" ->
            Just Juli

        "August" ->
            Just August

        "September" ->
            Just September

        "Oktober" ->
            Just Oktober

        "November" ->
            Just November

        "Desember" ->
            Just Desember

        _ ->
            Nothing


månedTilNummerMåned : Måned -> String
månedTilNummerMåned maaned =
    case maaned of
        Januar ->
            "01"

        Februar ->
            "02"

        Mars ->
            "03"

        April ->
            "04"

        Mai ->
            "05"

        Juni ->
            "06"

        Juli ->
            "07"

        August ->
            "08"

        September ->
            "09"

        Oktober ->
            "10"

        November ->
            "11"

        Desember ->
            "12"

        Ikke_valgt ->
            ""


validerÅr : String -> Bool
validerÅr string =
    case String.isEmpty string of
        True ->
            False

        False ->
            case String.toInt string of
                Just aar ->
                    aar
                        > 1000
                        && String.length string
                        == 4

                Nothing ->
                    False



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
            Just (År string)


feilmeldingÅr : String -> Maybe String
feilmeldingÅr år_ =
    if String.isEmpty år_ then
        Just "Skriv årstall"

    else if String.length år_ /= 4 || String.toInt år_ == Nothing then
        Just "Kun 4 siffer"

    else
        Nothing


decodeMonthYear : String -> Decoder ( Måned, År )
decodeMonthYear string =
    case String.split "-" string of
        årString :: månedString :: [] ->
            case ( stringTilÅr årString, nummerStringTilMåned månedString ) of
                ( Just år_, Just måned_ ) ->
                    Json.Decode.succeed ( måned_, år_ )

                _ ->
                    Json.Decode.fail ("Kan ikke decode YearMonth \"" ++ string ++ "\". Forventet streng på formen \"yyyy-mm\"")

        _ ->
            Json.Decode.fail ("Kan ikke decode YearMonth \"" ++ string ++ "\". Forventet streng på formen \"yyyy-mm\"")


nummerStringTilMåned : String -> Maybe Måned
nummerStringTilMåned string =
    case string of
        "01" ->
            Just Januar

        "02" ->
            Just Februar

        "03" ->
            Just Mars

        "04" ->
            Just April

        "05" ->
            Just Mai

        "06" ->
            Just Juni

        "07" ->
            Just Juli

        "08" ->
            Just August

        "09" ->
            Just September

        "10" ->
            Just Oktober

        "11" ->
            Just November

        "12" ->
            Just Desember

        _ ->
            Nothing


encodeMonthYear : Måned -> År -> Json.Encode.Value
encodeMonthYear måned_ (År år_) =
    Json.Encode.string (år_ ++ "-" ++ månedTilNummerMåned måned_)



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
    månedTilString måned_ ++ " " ++ årTilString år_
