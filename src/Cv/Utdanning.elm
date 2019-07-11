module Cv.Utdanning exposing
    ( Utdanning
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
    , nuskode : Maybe String
    , yrkesskole : Yrkesskole
    , harAutorisasjon : Bool -- Egentlig maybe?
    }


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


nuskode : Utdanning -> Maybe String
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


tilUtdanningsInfo : BackendData -> Decoder UtdanningInfo
tilUtdanningsInfo backendData =
    decodeYrkesskole backendData.yrkesskole
        |> map (lagUtdanningsinfo backendData)


lagUtdanningsinfo : BackendData -> Yrkesskole -> UtdanningInfo
lagUtdanningsinfo backendData ys =
    { id = backendData.id
    , studiested = backendData.studiested
    , utdanningsretning = backendData.utdanningsretning
    , fradato = backendData.fradato
    , tildato = backendData.tildato
    , beskrivelse = backendData.beskrivelse
    , navarende = backendData.navarende
    , nuskode = backendData.nuskode
    , yrkesskole = ys
    , harAutorisasjon = backendData.harAutorisasjon
    }


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
