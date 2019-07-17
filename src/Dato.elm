module Dato exposing (Måned(..))


type Dato
    = Dato DatoInfo


type alias DatoInfo =
    { måned : Måned
    , år : String
    , dag : String
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
                |> Maybe.withDefault "ERROR"
        , dag = "1"
        }
