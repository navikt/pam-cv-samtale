module Utdanning.Skjema exposing
    ( Felt(..)
    , UtdanningSkjema
    , ValidertUtdanningSkjema
    , ValidertUtdanningSkjemaInfo
    , encode
    , feilmeldingFraÅr
    , feilmeldingTilÅr
    , fraMåned
    , fraUtdanning
    , fraÅrValidert
    , gjørAlleFeilmeldingerSynlig
    , gjørFeilmeldingFraÅrSynlig
    , gjørFeilmeldingTilÅrSynlig
    , id
    , initValidertSkjema
    , innholdTekstFelt
    , nivå
    , nåværende
    , oppdaterFraMåned
    , oppdaterNivå
    , oppdaterTekstFelt
    , oppdaterTilMåned
    , tilDatoValidert
    , tilMåned
    , tilUvalidertSkjema
    , toggleNavarende
    , validerSkjema
    )

import Cv.Utdanning as Utdanning exposing (Nivå(..), Utdanning)
import Dato exposing (Måned(..), TilDato(..), År)
import Json.Encode
import Validering


type UtdanningSkjema
    = UtdanningSkjema UtdanningSkjemaInfo


type alias UtdanningSkjemaInfo =
    { nivå : Nivå
    , studiested : String
    , utdanningsretning : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : String
    , visFraÅrFeilmelding : Bool
    , nåværende : Bool
    , tilMåned : Måned
    , tilÅr : String
    , visTilÅrFeilmelding : Bool
    , id : Maybe String
    }



--- INIT ---


initValidertSkjema : ValidertUtdanningSkjemaInfo -> ValidertUtdanningSkjema
initValidertSkjema info =
    ValidertSkjema info


fraUtdanning : Utdanning -> UtdanningSkjema
fraUtdanning utdanning =
    UtdanningSkjema
        { nivå = Utdanning.nivå utdanning
        , studiested = Utdanning.studiested utdanning |> Maybe.withDefault ""
        , utdanningsretning = Utdanning.utdanningsretning utdanning |> Maybe.withDefault ""
        , beskrivelse = Utdanning.beskrivelse utdanning |> Maybe.withDefault ""
        , fraMåned = Utdanning.fraMåned utdanning
        , fraÅr = (Utdanning.fraÅr >> Dato.årTilString) utdanning
        , visFraÅrFeilmelding = False
        , nåværende = Utdanning.tilDato utdanning == Nåværende
        , tilMåned = (Utdanning.tilDato >> månedFraTilDato) utdanning
        , tilÅr = (Utdanning.tilDato >> årFraTilDato) utdanning
        , visTilÅrFeilmelding = False
        , id = Just (Utdanning.id utdanning)
        }


tilUvalidertSkjema : ValidertUtdanningSkjema -> UtdanningSkjema
tilUvalidertSkjema (ValidertSkjema info) =
    UtdanningSkjema
        { nivå = info.nivå
        , studiested = info.studiested
        , utdanningsretning = info.utdanningsretning
        , beskrivelse = info.beskrivelse
        , fraMåned = info.fraMåned
        , fraÅr = Dato.årTilString info.fraÅr
        , visFraÅrFeilmelding = False
        , nåværende = info.tilDato == Nåværende
        , tilMåned = månedFraTilDato info.tilDato
        , tilÅr = årFraTilDato info.tilDato
        , visTilÅrFeilmelding = False
        , id = info.id
        }


månedFraTilDato : TilDato -> Måned
månedFraTilDato tilDato =
    case tilDato of
        Nåværende ->
            Dato.Juni

        Avsluttet måned _ ->
            måned


årFraTilDato : TilDato -> String
årFraTilDato tilDato =
    case tilDato of
        Nåværende ->
            ""

        Avsluttet _ år ->
            Dato.årTilString år



--- INNHOLD ---


type Felt
    = Studiested
    | Utdanningsretning
    | Beskrivelse
    | FraÅr
    | TilÅr


innholdTekstFelt : Felt -> UtdanningSkjema -> String
innholdTekstFelt felt (UtdanningSkjema info) =
    case felt of
        Studiested ->
            info.studiested

        Utdanningsretning ->
            info.utdanningsretning

        Beskrivelse ->
            info.beskrivelse

        FraÅr ->
            info.fraÅr

        TilÅr ->
            info.tilÅr


nåværende : UtdanningSkjema -> Bool
nåværende (UtdanningSkjema info) =
    info.nåværende


nivå : UtdanningSkjema -> Nivå
nivå (UtdanningSkjema info) =
    info.nivå


fraMåned : UtdanningSkjema -> Måned
fraMåned (UtdanningSkjema info) =
    info.fraMåned


tilMåned : UtdanningSkjema -> Måned
tilMåned (UtdanningSkjema info) =
    info.tilMåned


id : UtdanningSkjema -> Maybe String
id (UtdanningSkjema info) =
    info.id


fraÅrValidert : ValidertUtdanningSkjema -> År
fraÅrValidert (ValidertSkjema info) =
    info.fraÅr


tilDatoValidert : ValidertUtdanningSkjema -> TilDato
tilDatoValidert (ValidertSkjema info) =
    info.tilDato



--- OPPDATERING ---


oppdaterTekstFelt : Felt -> String -> UtdanningSkjema -> UtdanningSkjema
oppdaterTekstFelt felt tekst (UtdanningSkjema skjema) =
    case felt of
        Studiested ->
            UtdanningSkjema { skjema | studiested = tekst }

        Utdanningsretning ->
            UtdanningSkjema { skjema | utdanningsretning = tekst }

        Beskrivelse ->
            UtdanningSkjema { skjema | beskrivelse = tekst }

        FraÅr ->
            UtdanningSkjema { skjema | fraÅr = tekst }

        TilÅr ->
            UtdanningSkjema { skjema | tilÅr = tekst }


oppdaterNivå : UtdanningSkjema -> Nivå -> UtdanningSkjema
oppdaterNivå (UtdanningSkjema skjema) nivå_ =
    UtdanningSkjema { skjema | nivå = nivå_ }


toggleNavarende : UtdanningSkjema -> UtdanningSkjema
toggleNavarende (UtdanningSkjema skjema) =
    UtdanningSkjema { skjema | nåværende = not skjema.nåværende }


oppdaterFraMåned : UtdanningSkjema -> Måned -> UtdanningSkjema
oppdaterFraMåned (UtdanningSkjema skjema) måned =
    UtdanningSkjema { skjema | fraMåned = måned }


oppdaterTilMåned : UtdanningSkjema -> Måned -> UtdanningSkjema
oppdaterTilMåned (UtdanningSkjema skjema) måned =
    UtdanningSkjema { skjema | tilMåned = måned }



--- FEILMELDINGER ---


feilmeldingFraÅr : UtdanningSkjema -> Maybe String
feilmeldingFraÅr (UtdanningSkjema skjema) =
    if skjema.visFraÅrFeilmelding then
        Dato.feilmeldingÅr skjema.fraÅr

    else
        Nothing


feilmeldingTilÅr : UtdanningSkjema -> Maybe String
feilmeldingTilÅr (UtdanningSkjema skjema) =
    if not skjema.nåværende && skjema.visTilÅrFeilmelding then
        Dato.feilmeldingÅr skjema.tilÅr

    else
        Nothing


gjørFeilmeldingFraÅrSynlig : UtdanningSkjema -> UtdanningSkjema
gjørFeilmeldingFraÅrSynlig (UtdanningSkjema skjema) =
    UtdanningSkjema { skjema | visFraÅrFeilmelding = True }


gjørFeilmeldingTilÅrSynlig : UtdanningSkjema -> UtdanningSkjema
gjørFeilmeldingTilÅrSynlig (UtdanningSkjema skjema) =
    UtdanningSkjema { skjema | visTilÅrFeilmelding = True }


gjørAlleFeilmeldingerSynlig : UtdanningSkjema -> UtdanningSkjema
gjørAlleFeilmeldingerSynlig skjema =
    skjema
        |> gjørFeilmeldingFraÅrSynlig
        |> gjørFeilmeldingTilÅrSynlig



--- VALIDERING ---


type ValidertUtdanningSkjema
    = ValidertSkjema ValidertUtdanningSkjemaInfo


type alias ValidertUtdanningSkjemaInfo =
    { nivå : Nivå
    , studiested : String
    , utdanningsretning : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    , tilDato : TilDato
    , id : Maybe String
    }


validerSkjema : UtdanningSkjema -> Maybe ValidertUtdanningSkjema
validerSkjema (UtdanningSkjema info) =
    if Validering.feilmeldingMaxAntallTegn info.beskrivelse 2000 /= Nothing then
        Nothing

    else
        Maybe.map2
            (\tilDato fraÅr ->
                ValidertSkjema
                    { nivå = info.nivå
                    , studiested = info.studiested
                    , utdanningsretning = info.utdanningsretning
                    , beskrivelse = String.trim info.beskrivelse
                    , fraMåned = info.fraMåned
                    , fraÅr = fraÅr
                    , id = info.id
                    , tilDato = tilDato
                    }
            )
            (validerTilDato info.nåværende info.tilMåned info.tilÅr)
            (Dato.stringTilÅr info.fraÅr)


validerTilDato : Bool -> Måned -> String -> Maybe TilDato
validerTilDato nåværende_ måned år =
    if nåværende_ then
        Just Nåværende

    else
        år
            |> Dato.stringTilÅr
            |> Maybe.map (Avsluttet måned)



--- ENCODING ---


encode : ValidertUtdanningSkjema -> Json.Encode.Value
encode (ValidertSkjema info) =
    [ [ ( "studiested", Json.Encode.string info.studiested )
      , ( "utdanningsretning", Json.Encode.string info.utdanningsretning )
      , ( "beskrivelse", Json.Encode.string info.beskrivelse )
      , ( "fradato", Dato.encodeMonthYear info.fraMåned info.fraÅr )
      , ( "nuskode", encodeNuskode info.nivå )
      ]
    , encodeTilDato info.tilDato
    ]
        |> List.concat
        |> Json.Encode.object


encodeTilDato : TilDato -> List ( String, Json.Encode.Value )
encodeTilDato tilDato =
    case tilDato of
        Nåværende ->
            [ ( "navarende", Json.Encode.bool True ) ]

        Avsluttet måned år ->
            [ ( "navarende", Json.Encode.bool False )
            , ( "tildato", Dato.encodeMonthYear måned år )
            ]


encodeNuskode : Nivå -> Json.Encode.Value
encodeNuskode nivå_ =
    case nivå_ of
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

        Doktorgrad ->
            Json.Encode.string "8"
