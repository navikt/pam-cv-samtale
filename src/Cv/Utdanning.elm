module Cv.Utdanning exposing
    ( Nivå(..)
    , Utdanning
    , Yrkesskole(..)
    , beskrivelse
    , decode
    , fradato
    , harAutorisasjon
    , id
    , navarende
    , nuskode
    , studiested
    , tildato
    , utdanningsretning
    , yrkesskole
    )

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Utdanning
    = Utdanning UtdanningInfo


type alias UtdanningInfo =
    { id : String
    , studiested : Maybe String
    , utdanningsretning : Maybe String
    , fradato : Maybe String
    , tildato : Maybe String
    , beskrivelse : Maybe String
    , navarende : Maybe Bool ----Maybe bool? ref Dtoen
    , nuskode : Nivå
    , yrkesskole : Yrkesskole
    , harAutorisasjon : Bool -- Egentlig maybe?
    }


type Nivå
    = Grunnskole
    | VideregåendeYrkesskole
    | Fagskole
    | Folkehøyskole
    | HøyereUtdanning1til4
    | HøyereUtdanning4pluss
    | Phd


type Yrkesskole
    = SvennebrevFagbrev
    | Mesterbrev
    | Ingen


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


fradato : Utdanning -> Maybe String
fradato (Utdanning info) =
    info.fradato


tildato : Utdanning -> Maybe String
tildato (Utdanning info) =
    info.tildato


navarende : Utdanning -> Maybe Bool
navarende (Utdanning info) =
    info.navarende


nuskode : Utdanning -> Nivå
nuskode (Utdanning info) =
    info.nuskode


yrkesskole : Utdanning -> Yrkesskole
yrkesskole (Utdanning info) =
    info.yrkesskole


harAutorisasjon : Utdanning -> Bool
harAutorisasjon (Utdanning info) =
    info.harAutorisasjon



---- Decoder ----


decode : Decoder Utdanning
decode =
    decodeBackendData
        |> andThen tilUtdanningsInfo
        |> map Utdanning


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "id" string
        |> required "studiested" (nullable string)
        |> required "utdanningsretning" (nullable string)
        |> required "fradato" (nullable string)
        |> required "tildato" (nullable string)
        |> required "beskrivelse" (nullable string)
        |> required "navarende" (nullable bool)
        |> required "nuskode" (nullable string)
        |> required "yrkesskole" string
        |> required "harAutorisasjon" bool



---decodeYrkesskole backendData.yrkesskole
---decodeNivå backendData.nuskode


tilUtdanningsInfo : BackendData -> Decoder UtdanningInfo
tilUtdanningsInfo backendData =
    map2 (lagUtdanningsinfo backendData)
        (decodeNivå backendData.nuskode)
        (decodeYrkesskole backendData.yrkesskole)


lagUtdanningsinfo : BackendData -> Nivå -> Yrkesskole -> UtdanningInfo
lagUtdanningsinfo backendData nivå ys =
    { id = backendData.id
    , studiested = backendData.studiested
    , utdanningsretning = backendData.utdanningsretning
    , fradato = backendData.fradato
    , tildato = backendData.tildato
    , beskrivelse = backendData.beskrivelse
    , navarende = backendData.navarende
    , nuskode = nivå
    , yrkesskole = ys
    , harAutorisasjon = backendData.harAutorisasjon
    }


decodeNivå : Maybe String -> Decoder Nivå
decodeNivå maybeNivå =
    case maybeNivå of
        Just nivå ->
            if nivå == "2" || String.left 1 nivå == "2" then
                succeed Grunnskole

            else if nivå == "3" || String.left 1 nivå == "3" then
                succeed VideregåendeYrkesskole

            else if nivå == "4" || String.left 1 nivå == "4" then
                succeed Fagskole

            else if nivå == "5" || String.left 1 nivå == "5" then
                succeed Folkehøyskole

            else if nivå == "6" || String.left 1 nivå == "6" then
                succeed HøyereUtdanning1til4

            else if nivå == "7" || String.left 1 nivå == "7" then
                succeed HøyereUtdanning4pluss

            else if nivå == "8" || String.left 1 nivå == "8" then
                succeed Phd

            else
                fail ("Decoding av enum Nivå feilet. Klarer ikke decode verdi: " ++ nivå)

        Nothing ->
            fail "Nuskode er et påkrevd felt, men var null"


decodeYrkesskole : String -> Decoder Yrkesskole
decodeYrkesskole ys =
    if ys == "SVENNEBREV_FAGBREV" then
        succeed SvennebrevFagbrev

    else if ys == "MESTERBREV" then
        succeed Mesterbrev

    else if ys == "INGEN" then
        succeed Ingen

    else
        fail ("Decoding av enum Yrkesskole feilet. Klarer ikke decode verdi: " ++ ys)


type alias BackendData =
    { id : String
    , studiested : Maybe String
    , utdanningsretning : Maybe String
    , fradato : Maybe String
    , tildato : Maybe String
    , beskrivelse : Maybe String
    , navarende : Maybe Bool ----Maybe bool? ref Dtoen
    , nuskode : Maybe String
    , yrkesskole : String --- Egentlig en enum
    , harAutorisasjon : Bool -- Egentlig maybe?
    }
