module Cv.Utdanning exposing
    ( Nivå(..)
    , Utdanning
    , beskrivelse
    , decode
    , fraMåned
    , fraÅr
    , id
    , nivå
    , studiested
    , tilDato
    , utdanningsretning
    )

import Dato exposing (TilDato(..), År)
import Dato.Maned exposing (Måned)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Utdanning
    = Utdanning UtdanningInfo


type alias UtdanningInfo =
    { id : String
    , studiested : Maybe String
    , utdanningsretning : Maybe String
    , fraMåned : Måned
    , fraÅr : År
    , tildato : TilDato
    , beskrivelse : Maybe String
    , nivå : Nivå
    }


type Nivå
    = Grunnskole
    | VideregåendeYrkesskole
    | Fagskole
    | Folkehøyskole
    | HøyereUtdanning1til4
    | HøyereUtdanning4pluss
    | Doktorgrad


id : Utdanning -> String
id (Utdanning info) =
    info.id


studiested : Utdanning -> Maybe String
studiested (Utdanning info) =
    info.studiested


utdanningsretning : Utdanning -> Maybe String
utdanningsretning (Utdanning info) =
    info.utdanningsretning


beskrivelse : Utdanning -> Maybe String
beskrivelse (Utdanning info) =
    info.beskrivelse


fraMåned : Utdanning -> Måned
fraMåned (Utdanning info) =
    info.fraMåned


fraÅr : Utdanning -> År
fraÅr (Utdanning info) =
    info.fraÅr


tilDato : Utdanning -> TilDato
tilDato (Utdanning info) =
    info.tildato


nivå : Utdanning -> Nivå
nivå (Utdanning info) =
    info.nivå



---- Decoder ----


decode : Decoder Utdanning
decode =
    decodeBackendData
        |> Json.Decode.andThen tilUtdanningsInfo


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "id" string
        |> required "studiested" (nullable string)
        |> required "utdanningsretning" (nullable string)
        |> required "fradato" string
        |> required "tildato" (nullable string)
        |> required "beskrivelse" (nullable string)
        |> required "navarende" bool
        |> required "nuskode" string


tilUtdanningsInfo : BackendData -> Decoder Utdanning
tilUtdanningsInfo backendData =
    Json.Decode.map3 (lagUtdanning backendData)
        (decodeNivå backendData.nuskode)
        (Dato.decodeMonthYear backendData.fradato)
        (decodeTilDato backendData.navarende backendData.tildato)


lagUtdanning : BackendData -> Nivå -> ( Måned, År ) -> TilDato -> Utdanning
lagUtdanning backendData nivå_ ( fraMåned_, fraÅr_ ) tilDato_ =
    Utdanning
        { id = backendData.id
        , studiested = backendData.studiested
        , utdanningsretning = backendData.utdanningsretning
        , fraMåned = fraMåned_
        , fraÅr = fraÅr_
        , tildato = tilDato_
        , beskrivelse = backendData.beskrivelse
        , nivå = nivå_
        }


decodeNivå : String -> Decoder Nivå
decodeNivå nivå_ =
    if String.left 1 nivå_ == "1" || String.left 1 nivå_ == "2" then
        succeed Grunnskole

    else if String.left 1 nivå_ == "4" then
        succeed VideregåendeYrkesskole

    else if String.left 1 nivå_ == "5" then
        succeed Fagskole

    else if String.left 1 nivå_ == "3" then
        succeed Folkehøyskole

    else if String.left 1 nivå_ == "6" then
        succeed HøyereUtdanning1til4

    else if String.left 1 nivå_ == "7" then
        succeed HøyereUtdanning4pluss

    else if String.left 1 nivå_ == "8" then
        succeed Doktorgrad

    else
        fail ("Decoding av nuskode feilet. Klarer ikke decode verdi: " ++ nivå_)


decodeTilDato : Bool -> Maybe String -> Decoder TilDato
decodeTilDato nåværende maybeTilDatoString =
    if nåværende then
        succeed Nåværende

    else
        case maybeTilDatoString of
            Just tilDatoString ->
                tilDatoString
                    |> Dato.decodeMonthYear
                    |> Json.Decode.map (\( a, b ) -> Avsluttet a b)

            Nothing ->
                fail "Decoding av til-dato feilet. nåværende kan ikke være false samtidig som tildato er null"


type alias BackendData =
    -- TODO: Endre DTO på backend til å kun være optional på riktige felter
    { id : String
    , studiested : Maybe String
    , utdanningsretning : Maybe String
    , fradato : String
    , tildato : Maybe String
    , beskrivelse : Maybe String
    , navarende : Bool
    , nuskode : String
    }
