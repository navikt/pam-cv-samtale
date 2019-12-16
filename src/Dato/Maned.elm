module Dato.Maned exposing (Måned(..), fraNummer, fraString, måneder, stringTilMåned, tilNummer, tilString)


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


måneder : List Måned
måneder =
    [ Januar
    , Februar
    , Mars
    , April
    , Mai
    , Juni
    , Juli
    , August
    , September
    , Oktober
    , November
    , Desember
    ]


tilString : Måned -> String
tilString mnd =
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


fraString : String -> Maybe Måned
fraString string =
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


tilNummer : Måned -> String
tilNummer maaned =
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


fraNummer : String -> Maybe Måned
fraNummer string =
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
