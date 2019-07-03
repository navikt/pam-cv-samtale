module Cv.Utdanning exposing
    ( Utdanning
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
    , yrkesskole : String --- Egentlig en enum
    , harAutorisasjon : Bool -- Egentlig maybe?
    }



{--
Utdanninger:
data class UtdanningDto(
                    val id: String?,
                    val studiested: String?,
                    val utdanningsretning: String?,
                    val fradato: String?,
                    val tildato: String?,
                    val beskrivelse: String?,
                    val navarende: Boolean?,
                    val nuskode: String?,
                    val yrkesskole: Yrkesskole? = Yrkesskole.INGEN,
                    val harAutorisasjon: Boolean? = false

--}


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


yrkesskole : Utdanning -> String
yrkesskole (Utdanning info) =
    info.yrkesskole


harAutorisasjon : Utdanning -> Bool
harAutorisasjon (Utdanning info) =
    info.harAutorisasjon



---- Decoder ----


decode : Decoder Utdanning
decode =
    decodeBackendData
        |> map Utdanning


decodeBackendData : Decoder UtdanningInfo
decodeBackendData =
    succeed UtdanningInfo
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
