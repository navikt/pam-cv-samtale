module Skjema.Utdanning exposing (Felt(..), UtdanningSkjema, beskrivelse, encode, fradato, init, navarende, oppdaterFelt, studiested, tildato, utdanningsretning)

import Cv.Utdanning exposing (Nivå(..), Utdanning, Yrkesskole(..))
import Json.Encode


type UtdanningSkjema
    = UtdanningSkjema UtdanningSkjemaInfo


type alias UtdanningSkjemaInfo =
    { studiested : String
    , utdanningsretning : String
    , fradato : String
    , tildato : String
    , beskrivelse : String
    , navarende : Bool --- bool? ref Dtoen
    , nuskode : Nivå
    }


type Felt
    = Studiested
    | Utdanningsretning
    | Beskrivelse
    | Fradato
    | Tildato
    | Navarende
    | Nuskode


studiested : UtdanningSkjema -> String
studiested (UtdanningSkjema info) =
    info.studiested


utdanningsretning : UtdanningSkjema -> String
utdanningsretning (UtdanningSkjema info) =
    info.utdanningsretning


fradato : UtdanningSkjema -> String
fradato (UtdanningSkjema info) =
    info.fradato


tildato : UtdanningSkjema -> String
tildato (UtdanningSkjema info) =
    info.tildato


beskrivelse : UtdanningSkjema -> String
beskrivelse (UtdanningSkjema info) =
    info.beskrivelse


navarende : UtdanningSkjema -> Bool
navarende (UtdanningSkjema info) =
    info.navarende


init : Utdanning -> UtdanningSkjema
init utdanning =
    UtdanningSkjema
        { studiested = Cv.Utdanning.studiested utdanning |> Maybe.withDefault ""
        , utdanningsretning = Cv.Utdanning.utdanningsretning utdanning |> Maybe.withDefault ""
        , beskrivelse = Cv.Utdanning.beskrivelse utdanning |> Maybe.withDefault ""
        , fradato = Cv.Utdanning.fradato utdanning |> Maybe.withDefault ""
        , tildato = Cv.Utdanning.tildato utdanning |> Maybe.withDefault ""
        , navarende = Cv.Utdanning.navarende utdanning |> Maybe.withDefault False
        , nuskode = Grunnskole
        }



{--
oppdaterFelt : Felt -> UtdanningSkjema -> ( String, Bool ) -> UtdanningSkjema
oppdaterFelt felt (UtdanningSkjema info) ( str, bol ) =
    case felt of
        Studiested ->
            UtdanningSkjema { info | studiested = str }

        Utdanningsretning ->
            UtdanningSkjema { info | utdanningsretning = str }

        Beskrivelse ->
            UtdanningSkjema { info | beskrivelse = str }

        Fradato ->
            UtdanningSkjema { info | fradato = str }

        Tildato ->
            UtdanningSkjema { info | tildato = str }

        Navarende ->
            UtdanningSkjema { info | navarende = bol }

        Nuskode ->
            UtdanningSkjema { info | nuskode = str }
--}


oppdaterFelt : Felt -> UtdanningSkjema -> ( String, Bool ) -> UtdanningSkjema
oppdaterFelt felt skjema ( str, bol ) =
    skjema


encode : UtdanningSkjema -> String -> Nivå -> Json.Encode.Value
encode (UtdanningSkjema info) id nivå =
    Json.Encode.object
        [ ( "id", Json.Encode.string id )
        , ( "studiested", Json.Encode.string info.studiested )
        , ( "utdannningsretning", Json.Encode.string info.utdanningsretning )
        , ( "beskrivelse", Json.Encode.string info.beskrivelse )
        , ( "fradato", Json.Encode.string info.fradato )
        , ( "tildato", Json.Encode.string info.tildato )
        , ( "navarende", Json.Encode.bool info.navarende )
        , ( "nuskode", encodeNuskode nivå )
        ]


encodeNuskode : Nivå -> Json.Encode.Value
encodeNuskode nivå =
    case nivå of
        Grunnskole ->
            Json.Encode.string "2"

        VideregåendeYrkesskole ->
            Json.Encode.string "3"

        Fagskole ->
            Json.Encode.string "4"

        Folkehøyskole ->
            Json.Encode.string "5"

        HøyereUtdanning1til4 ->
            Json.Encode.string "6"

        HøyereUtdanning4pluss ->
            Json.Encode.string "7"

        Phd ->
            Json.Encode.string "8"

        Ukjent ->
            Json.Encode.string ""
