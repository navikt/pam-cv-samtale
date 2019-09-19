module Dato exposing
    ( Dato
    , Måned(..)
    , År
    , decodeMonthYear
    , encodeMonthYear
    , feilmeldingÅr
    , fraStringTilDato
    , måned
    , månedTilNummerMåned
    , månedTilString
    , setMåned
    , setÅr
    , stringTilMåned
    , stringTilÅr
    , tilStringForBackend
    , validerÅr
    , år
    , årTilString
    )

import Json.Decode exposing (Decoder)
import Json.Encode


type Dato
    = Dato DatoInfo


type alias DatoInfo =
    { måned : Måned
    , år : Int
    , dag : Int
    }


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


måned : Dato -> Måned
måned (Dato info) =
    info.måned


år : Dato -> Int
år (Dato info) =
    info.år


setMåned : Dato -> Måned -> Dato
setMåned (Dato info) maaned =
    Dato { info | måned = maaned }


setÅr : Dato -> Int -> Dato
setÅr (Dato info) aar =
    Dato { info | år = aar }


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



-- Tar en string av typen 2006-09


fraStringTilDato : String -> Dato
fraStringTilDato string =
    case String.split "-" string of
        aar :: maaned :: [] ->
            case String.toInt aar of
                Just aarInt ->
                    Dato
                        { måned = stringTilMåned maaned
                        , år = aarInt
                        , dag = 1
                        }

                _ ->
                    Dato { måned = Januar, år = 9999, dag = 1 }

        _ ->
            Dato { måned = Januar, år = 9999, dag = 1 }


tilStringForBackend : Dato -> String
tilStringForBackend (Dato info) =
    String.fromInt info.år ++ "-" ++ månedTilNummerMåned info.måned


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
        Just "Skriv inn et årstall"

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
