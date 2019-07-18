module Dato exposing (Måned(..), månedTilString)


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


månedTilString : Måned -> String
månedTilString måned =
    case måned of
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
