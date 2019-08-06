module Skjema.Utdanning exposing
    ( Felt(..)
    , UtdanningSkjema(..)
    , UtdanningsSkjemaVerdier
    , beskrivelse
    , encode
    ,  fraDato
       --, init

    , initManueltSkjema
    , navarende
    , nuskode
    , oppdaterBeskrivelse
    , oppdaterFraMåned
    , oppdaterFraÅr
    , oppdaterNavarende
    , oppdaterNavarendeFelt
    , oppdaterNuskode
    , oppdaterStudiested
    , oppdaterTilMåned
    , oppdaterTilÅr
    , oppdaterUtdanningsretning
    , studiested
    , tilDato
    , toggleBool
    , utdanningsretning
    )

import Cv.Utdanning exposing (Nivå(..), Utdanning, Yrkesskole(..))
import Dato exposing (Dato)
import Json.Encode


type UtdanningSkjema
    = UtdanningSkjema UtdanningSkjemaInfo


type alias UtdanningSkjemaInfo =
    { studiested : String
    , utdanningsretning : String
    , fradato : Dato
    , tildato : Maybe Dato
    , beskrivelse : String
    , navarende : Bool
    , nuskode : Nivå
    }


type Felt
    = Studiested
    | Utdanningsretning
    | Beskrivelse
    | FraMåned
    | FraÅr
    | Navarende
    | TilMåned
    | TilÅr
    | Nuskode


studiested : UtdanningSkjema -> String
studiested (UtdanningSkjema info) =
    info.studiested


utdanningsretning : UtdanningSkjema -> String
utdanningsretning (UtdanningSkjema info) =
    info.utdanningsretning


beskrivelse : UtdanningSkjema -> String
beskrivelse (UtdanningSkjema info) =
    info.beskrivelse


navarende : UtdanningSkjema -> Bool
navarende (UtdanningSkjema info) =
    info.navarende


fraDato : UtdanningSkjema -> Dato
fraDato (UtdanningSkjema info) =
    info.fradato


tilDato : UtdanningSkjema -> Maybe Dato
tilDato (UtdanningSkjema info) =
    info.tildato


nuskode : UtdanningSkjema -> Nivå
nuskode (UtdanningSkjema info) =
    info.nuskode



{--
init : Utdanning -> Dato -> UtdanningSkjema
init utdanning =
    UtdanningSkjema
        { studiested = Cv.Utdanning.studiested utdanning |> Maybe.withDefault ""
        , utdanningsretning = Cv.Utdanning.utdanningsretning utdanning |> Maybe.withDefault ""
        , beskrivelse = Cv.Utdanning.beskrivelse utdanning |> Maybe.withDefault ""
        , fradato = Dato.tilDato (Cv.Utdanning.fradato utdanning)
        , tildato = Dato.tilDato (Cv.Utdanning.tildato utdanning)
        , navarende = Cv.Utdanning.navarende utdanning |> Maybe.withDefault False
        , nuskode = Cv.Utdanning.nuskode utdanning
        }
--}


initManueltSkjema : UtdanningsSkjemaVerdier -> UtdanningSkjema
initManueltSkjema info =
    UtdanningSkjema info


type alias UtdanningsSkjemaVerdier =
    { studiested : String
    , utdanningsretning : String
    , fradato : Dato
    , tildato : Maybe Dato
    , beskrivelse : String
    , navarende : Bool
    , nuskode : Nivå
    }


oppdaterNuskode : Nivå -> UtdanningSkjema -> UtdanningSkjema
oppdaterNuskode nivå (UtdanningSkjema skjema) =
    UtdanningSkjema { skjema | nuskode = nivå }


oppdaterStudiested : String -> UtdanningSkjema -> UtdanningSkjema
oppdaterStudiested skole (UtdanningSkjema skjema) =
    UtdanningSkjema { skjema | studiested = skole }


oppdaterUtdanningsretning : String -> UtdanningSkjema -> UtdanningSkjema
oppdaterUtdanningsretning retning (UtdanningSkjema skjema) =
    UtdanningSkjema { skjema | utdanningsretning = retning }


oppdaterBeskrivelse : String -> UtdanningSkjema -> UtdanningSkjema
oppdaterBeskrivelse beskr (UtdanningSkjema skjema) =
    UtdanningSkjema { skjema | beskrivelse = beskr }


oppdaterNavarende : Bool -> UtdanningSkjema -> UtdanningSkjema
oppdaterNavarende bool (UtdanningSkjema skjema) =
    if skjema.navarende == True then
        UtdanningSkjema
            { skjema
                | tildato =
                    tilDato (UtdanningSkjema skjema)
                        |> Maybe.withDefault (Dato.fraStringTilDato "1970-01")
                        |> Just
                , navarende = bool
            }

    else
        UtdanningSkjema { skjema | navarende = bool }


oppdaterFraMåned : String -> UtdanningSkjema -> UtdanningSkjema
oppdaterFraMåned string (UtdanningSkjema skjema) =
    UtdanningSkjema
        { skjema
            | fradato =
                string
                    |> Dato.stringTilMåned
                    |> Dato.setMåned skjema.fradato
        }


oppdaterFraÅr : String -> UtdanningSkjema -> UtdanningSkjema
oppdaterFraÅr string (UtdanningSkjema skjema) =
    UtdanningSkjema
        { skjema
            | fradato =
                string
                    |> String.toInt
                    |> Maybe.withDefault 0
                    |> Dato.setÅr skjema.fradato
        }


oppdaterNavarendeFelt : UtdanningSkjema -> UtdanningSkjema
oppdaterNavarendeFelt (UtdanningSkjema skjema) =
    if skjema.navarende == True then
        UtdanningSkjema { skjema | navarende = False }

    else
        UtdanningSkjema { skjema | navarende = True }


oppdaterTilMåned : String -> UtdanningSkjema -> UtdanningSkjema
oppdaterTilMåned string (UtdanningSkjema skjema) =
    case skjema.tildato of
        Just dato ->
            UtdanningSkjema
                { skjema
                    | tildato =
                        string
                            |> Dato.stringTilMåned
                            |> Dato.setMåned dato
                            |> Just
                }

        Nothing ->
            UtdanningSkjema skjema


oppdaterTilÅr : String -> UtdanningSkjema -> UtdanningSkjema
oppdaterTilÅr string (UtdanningSkjema skjema) =
    case skjema.tildato of
        Just dato ->
            UtdanningSkjema
                { skjema
                    | tildato =
                        string
                            |> String.toInt
                            |> Maybe.withDefault 0
                            |> Dato.setÅr dato
                            |> Just
                }

        Nothing ->
            UtdanningSkjema
                skjema



{--
oppdaterStringFelt : UtdanningSkjema -> Felt -> String -> UtdanningSkjema
oppdaterStringFelt skjema felt string =
    case felt of
        Yrke ->
            string
                |> oppdaterYrkeFelt skjema

        JobbTittel ->
            string
                |> oppdaterJobbTittelFelt skjema

        BedriftNavn ->
            string
                |> oppdaterBedriftNavnFelt skjema

        Lokasjon ->
            string
                |> oppdaterLokasjonFelt skjema

        Arbeidsoppgaver ->
            string
                |> oppdaterArbeidsoppgaverFelt skjema

        FraMåned ->
            string
                |> oppdaterFraMåned skjema

        FraÅr ->
            if string == "" then
                oppdaterFraÅr skjema string

            else
                case String.toInt string of
                    Just a ->
                        oppdaterFraÅr skjema string

                    Nothing ->
                        skjema

        TilMåned ->
            string
                |> oppdaterTilMåned skjema

        TilÅr ->
            string
                |> oppdaterTilÅr skjema

        _ ->
            skjema
--}


toggleBool : UtdanningSkjema -> Felt -> UtdanningSkjema
toggleBool (UtdanningSkjema skjema) felt =
    case felt of
        Navarende ->
            let
                nyVerdi =
                    skjema.navarende
            in
            UtdanningSkjema { skjema | navarende = not nyVerdi }

        _ ->
            UtdanningSkjema skjema



{--
oppdaterFelt : Felt -> UtdanningSkjema -> ( String, Bool, Nivå ) -> UtdanningSkjema
oppdaterFelt felt (UtdanningSkjema info) ( str, bol, nivå ) =
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
            UtdanningSkjema { info | nuskode = nivå }
--}


encode : UtdanningSkjema -> String -> Nivå -> Json.Encode.Value
encode (UtdanningSkjema info) id nivå =
    case info.tildato of
        Just dato ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "studiested", Json.Encode.string info.studiested )
                , ( "utdanningsretning", Json.Encode.string info.utdanningsretning )
                , ( "beskrivelse", Json.Encode.string info.beskrivelse )
                , ( "fradato"
                  , info.fradato
                        |> Dato.tilStringForBackend
                        |> Json.Encode.string
                  )
                , ( "tildato"
                  , dato
                        |> Dato.tilStringForBackend
                        |> Json.Encode.string
                  )
                , ( "navarende", Json.Encode.bool info.navarende )
                , ( "nuskode", encodeNuskode nivå )
                ]

        Nothing ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "studiested", Json.Encode.string info.studiested )
                , ( "utdanningsretning", Json.Encode.string info.utdanningsretning )
                , ( "beskrivelse", Json.Encode.string info.beskrivelse )
                , ( "fradato", Json.Encode.string (info.fradato |> Dato.tilStringForBackend) )
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
