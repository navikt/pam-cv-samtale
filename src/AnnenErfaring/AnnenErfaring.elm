module AnnenErfaring.AnnenErfaring exposing (AnnenErfaring, beskrivelse, decode, fradato, id, naavaerende, rolle, tildato)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type AnnenErfaring
    = AnnenErfaring AnnenErfaringInfo


type alias AnnenErfaringInfo =
    { id : String
    , fradato : Maybe String
    , tildato : Maybe String
    , naavaerende : Maybe Bool
    , beskrivelse : Maybe String
    , rolle : Maybe String
    }


id : AnnenErfaring -> String
id (AnnenErfaring info) =
    info.id


naavaerende : AnnenErfaring -> Maybe Bool
naavaerende (AnnenErfaring info) =
    info.naavaerende


beskrivelse : AnnenErfaring -> Maybe String
beskrivelse (AnnenErfaring info) =
    info.beskrivelse


rolle : AnnenErfaring -> Maybe String
rolle (AnnenErfaring info) =
    info.rolle


fradato : AnnenErfaring -> Maybe String
fradato (AnnenErfaring info) =
    info.fradato


tildato : AnnenErfaring -> Maybe String
tildato (AnnenErfaring info) =
    info.tildato



---- Decoder ----


decode : Decoder AnnenErfaring
decode =
    decodeBackendData
        |> map AnnenErfaring


decodeBackendData : Decoder AnnenErfaringInfo
decodeBackendData =
    succeed AnnenErfaringInfo
        |> required "id" string
        |> required "fradato" (nullable string)
        |> required "tildato" (nullable string)
        |> required "naavaerende" (nullable bool)
        |> required "beskrivelse" (nullable string)
        |> required "rolle" (nullable string)
