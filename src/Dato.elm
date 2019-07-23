module Dato exposing (Dato, Måned(..), måned, månedTilString, tilDato, validerÅr, år)


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


tilDato : String -> Dato
tilDato string =
    let
        list =
            String.split "-" string
    in
    Dato
        { måned = Januar
        , år =
            list
                |> List.head
                |> Maybe.withDefault "0"
                |> String.toInt
                |> Maybe.withDefault 0
        , dag =
            String.toInt "1"
                |> Maybe.withDefault 0
        }


toString : Dato -> String
toString (Dato info) =
    String.fromInt info.år ++ "-" ++ månedTilString info.måned


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
