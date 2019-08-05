module Dato exposing (Dato, Måned(..), måned, månedTilString, setMåned, setÅr, stringTilMåned, tilDato, tilString, tilStringForBackend, validerÅr, år)


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


tilDato : String -> Dato
tilDato string =
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


tilString : Dato -> String
tilString (Dato info) =
    String.fromInt info.år ++ "-" ++ månedTilString info.måned


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
            "05"

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


validerAarMaanedDag : String -> Bool
validerAarMaanedDag string =
    case String.split "-" string of
        aar :: maaned :: dag :: [] ->
            case ( String.toInt aar, String.toInt maaned, String.toInt dag ) of
                ( Just aarInt, Just maanedInt, Just dagInt ) ->
                    aarInt
                        > 1900
                        && maanedInt
                        > 0
                        && maanedInt
                        < 13

                _ ->
                    False

        _ ->
            False


validerAarMaaned : String -> Bool
validerAarMaaned string =
    case String.split "-" string of
        aar :: maaned :: dag :: [] ->
            case ( String.toInt aar, String.toInt maaned, String.toInt dag ) of
                ( Just aarInt, Just maanedInt, Just dagInt ) ->
                    aarInt
                        > 1900
                        && maanedInt
                        > 0
                        && maanedInt
                        < 13

                _ ->
                    False

        _ ->
            False
