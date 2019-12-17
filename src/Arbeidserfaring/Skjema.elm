module Arbeidserfaring.Skjema exposing
    ( ArbeidserfaringSkjema
    , Felt(..)
    , SkjemaInfo
    , ValidertArbeidserfaringSkjema
    , encode
    , feilmeldingFraÅr
    , feilmeldingTilÅr
    , feilmeldingYrke
    , fraArbeidserfaring
    , fraMåned
    , fraÅrValidert
    , gjørAlleFeilmeldingerSynlig
    , gjørFeilmeldingFraÅrSynlig
    , gjørFeilmeldingTilÅrSynlig
    , gjørFeilmeldingYrkeSynlig
    , id
    , initValidertSkjema
    , innholdTekstFelt
    , nåværende
    , oppdaterFraMåned
    , oppdaterStringFelt
    , oppdaterTilMåned
    , oppdaterYrke
    , tilDatoValidert
    , tilMåned
    , tilUvalidertSkjema
    , toggleNåværende
    , valider
    , yrke
    )

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Dato.Dato as Dato exposing (TilDato(..), År)
import Dato.Maned exposing (Måned(..))
import Json.Encode
import Validering


type ArbeidserfaringSkjema
    = ArbeidserfaringSkjema SkjemaInfo


type alias SkjemaInfo =
    { yrke : Maybe Yrke
    , visYrkeFeilmelding : Bool
    , jobbTittel : String
    , bedriftsnavn : String
    , sted : String
    , arbeidsoppgaver : String
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


initValidertSkjema : ValidertSkjemaInfo -> ValidertArbeidserfaringSkjema
initValidertSkjema skjemaInfo =
    ValidertArbeidserfaringSkjema skjemaInfo


fraArbeidserfaring : Arbeidserfaring -> ArbeidserfaringSkjema
fraArbeidserfaring arbeidserfaring =
    ArbeidserfaringSkjema
        { yrke = Arbeidserfaring.yrke arbeidserfaring
        , visYrkeFeilmelding = False
        , jobbTittel = (Arbeidserfaring.yrkeFritekst >> Maybe.withDefault "") arbeidserfaring
        , bedriftsnavn = (Arbeidserfaring.arbeidsgiver >> Maybe.withDefault "") arbeidserfaring
        , sted = (Arbeidserfaring.sted >> Maybe.withDefault "") arbeidserfaring
        , arbeidsoppgaver = (Arbeidserfaring.beskrivelse >> Maybe.withDefault "") arbeidserfaring
        , fraMåned = Arbeidserfaring.fraMåned arbeidserfaring
        , fraÅr = (Arbeidserfaring.fraÅr >> Dato.årTilString) arbeidserfaring
        , visFraÅrFeilmelding = False
        , nåværende = Arbeidserfaring.tilDato arbeidserfaring == Nåværende
        , tilMåned = (Arbeidserfaring.tilDato >> månedFraTilDato) arbeidserfaring
        , tilÅr = (Arbeidserfaring.tilDato >> årFraTilDato) arbeidserfaring
        , visTilÅrFeilmelding = False
        , id = (Arbeidserfaring.id >> Just) arbeidserfaring
        }


tilUvalidertSkjema : ValidertArbeidserfaringSkjema -> ArbeidserfaringSkjema
tilUvalidertSkjema (ValidertArbeidserfaringSkjema info) =
    ArbeidserfaringSkjema
        { yrke = Just info.yrke
        , visYrkeFeilmelding = False
        , jobbTittel = info.jobbTittel
        , bedriftsnavn = info.bedriftNavn
        , sted = info.lokasjon
        , arbeidsoppgaver = info.arbeidsoppgaver
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
            Januar

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
    = Jobbtittel
    | Bedriftsnavn
    | Sted
    | Arbeidsoppgaver
    | FraÅr
    | TilÅr


innholdTekstFelt : Felt -> ArbeidserfaringSkjema -> String
innholdTekstFelt felt (ArbeidserfaringSkjema skjema) =
    case felt of
        Jobbtittel ->
            skjema.jobbTittel

        Bedriftsnavn ->
            skjema.bedriftsnavn

        Sted ->
            skjema.sted

        Arbeidsoppgaver ->
            skjema.arbeidsoppgaver

        FraÅr ->
            skjema.fraÅr

        TilÅr ->
            skjema.tilÅr


nåværende : ArbeidserfaringSkjema -> Bool
nåværende (ArbeidserfaringSkjema info) =
    info.nåværende


fraMåned : ArbeidserfaringSkjema -> Måned
fraMåned (ArbeidserfaringSkjema info) =
    info.fraMåned


tilMåned : ArbeidserfaringSkjema -> Måned
tilMåned (ArbeidserfaringSkjema info) =
    info.tilMåned


yrke : ValidertArbeidserfaringSkjema -> Yrke
yrke (ValidertArbeidserfaringSkjema info) =
    info.yrke


fraÅrValidert : ValidertArbeidserfaringSkjema -> År
fraÅrValidert (ValidertArbeidserfaringSkjema info) =
    info.fraÅr


tilDatoValidert : ValidertArbeidserfaringSkjema -> TilDato
tilDatoValidert (ValidertArbeidserfaringSkjema info) =
    info.tilDato


id : ValidertArbeidserfaringSkjema -> Maybe String
id (ValidertArbeidserfaringSkjema info) =
    info.id



--- OPPDATERING ---


oppdaterStringFelt : Felt -> String -> ArbeidserfaringSkjema -> ArbeidserfaringSkjema
oppdaterStringFelt felt string (ArbeidserfaringSkjema skjema) =
    case felt of
        Jobbtittel ->
            ArbeidserfaringSkjema { skjema | jobbTittel = string }

        Bedriftsnavn ->
            ArbeidserfaringSkjema { skjema | bedriftsnavn = string }

        Sted ->
            ArbeidserfaringSkjema { skjema | sted = string }

        Arbeidsoppgaver ->
            ArbeidserfaringSkjema { skjema | arbeidsoppgaver = string }

        FraÅr ->
            ArbeidserfaringSkjema { skjema | fraÅr = string }

        TilÅr ->
            ArbeidserfaringSkjema { skjema | tilÅr = string }


toggleNåværende : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
toggleNåværende (ArbeidserfaringSkjema skjema) =
    ArbeidserfaringSkjema { skjema | nåværende = not skjema.nåværende }


oppdaterFraMåned : ArbeidserfaringSkjema -> Måned -> ArbeidserfaringSkjema
oppdaterFraMåned (ArbeidserfaringSkjema skjema) fraMåned_ =
    ArbeidserfaringSkjema { skjema | fraMåned = fraMåned_ }


oppdaterTilMåned : ArbeidserfaringSkjema -> Måned -> ArbeidserfaringSkjema
oppdaterTilMåned (ArbeidserfaringSkjema skjema) tilMåned_ =
    ArbeidserfaringSkjema { skjema | tilMåned = tilMåned_ }


oppdaterYrke : ArbeidserfaringSkjema -> Maybe Yrke -> ArbeidserfaringSkjema
oppdaterYrke (ArbeidserfaringSkjema skjema) yrke_ =
    ArbeidserfaringSkjema { skjema | yrke = yrke_ }



--- FEILMELDINGER ---


feilmeldingYrke : ArbeidserfaringSkjema -> Maybe String
feilmeldingYrke (ArbeidserfaringSkjema skjema) =
    if skjema.visYrkeFeilmelding && skjema.yrke == Nothing then
        Just "Velg et yrke fra listen med forslag som kommer opp"

    else
        Nothing


feilmeldingFraÅr : ArbeidserfaringSkjema -> Maybe String
feilmeldingFraÅr (ArbeidserfaringSkjema skjema) =
    if skjema.visFraÅrFeilmelding then
        Dato.feilmeldingÅr skjema.fraÅr

    else
        Nothing


feilmeldingTilÅr : ArbeidserfaringSkjema -> Maybe String
feilmeldingTilÅr (ArbeidserfaringSkjema skjema) =
    if not skjema.nåværende && skjema.visTilÅrFeilmelding then
        Dato.feilmeldingÅr skjema.tilÅr

    else
        Nothing


gjørFeilmeldingYrkeSynlig : Bool -> ArbeidserfaringSkjema -> ArbeidserfaringSkjema
gjørFeilmeldingYrkeSynlig synlig (ArbeidserfaringSkjema skjema) =
    -- Skal alltid vises etter onBlur/onSubmit, så hvis den noen gang har vært True, skal den alltid fortsette å være True
    ArbeidserfaringSkjema { skjema | visYrkeFeilmelding = synlig || skjema.visYrkeFeilmelding }


gjørFeilmeldingFraÅrSynlig : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
gjørFeilmeldingFraÅrSynlig (ArbeidserfaringSkjema skjema) =
    ArbeidserfaringSkjema { skjema | visFraÅrFeilmelding = True }


gjørFeilmeldingTilÅrSynlig : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
gjørFeilmeldingTilÅrSynlig (ArbeidserfaringSkjema skjema) =
    ArbeidserfaringSkjema { skjema | visTilÅrFeilmelding = True }


gjørAlleFeilmeldingerSynlig : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
gjørAlleFeilmeldingerSynlig skjema =
    skjema
        |> gjørFeilmeldingYrkeSynlig True
        |> gjørFeilmeldingFraÅrSynlig
        |> gjørFeilmeldingTilÅrSynlig



--- VALIDERING ---


type ValidertArbeidserfaringSkjema
    = ValidertArbeidserfaringSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { yrke : Yrke
    , jobbTittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsoppgaver : String
    , fraMåned : Måned
    , fraÅr : År
    , tilDato : TilDato
    , id : Maybe String
    }


valider : ArbeidserfaringSkjema -> Maybe ValidertArbeidserfaringSkjema
valider (ArbeidserfaringSkjema info) =
    -- TODO: Hvorfor sjekker man om Yrke.label er tom streng?
    --            if Yrke.label yrkefelt /= "" then
    if Validering.feilmeldingMaxAntallTegn info.arbeidsoppgaver 2000 /= Nothing then
        Nothing

    else
        Maybe.map3
            (\yrke_ tilDato fraÅr_ ->
                ValidertArbeidserfaringSkjema
                    { yrke = yrke_
                    , jobbTittel = info.jobbTittel
                    , bedriftNavn = info.bedriftsnavn
                    , lokasjon = info.sted
                    , arbeidsoppgaver = info.arbeidsoppgaver
                    , fraMåned = info.fraMåned
                    , fraÅr = fraÅr_
                    , tilDato = tilDato
                    , id = info.id
                    }
            )
            info.yrke
            (validerTilDato info.nåværende info.tilMåned info.tilÅr)
            (Dato.stringTilÅr info.fraÅr)



--            else
--                Nothing


validerTilDato : Bool -> Måned -> String -> Maybe TilDato
validerTilDato nåværende_ måned år =
    if nåværende_ then
        Just Nåværende

    else
        år
            |> Dato.stringTilÅr
            |> Maybe.map (Avsluttet måned)



--- ENCODING ---


encode : ValidertArbeidserfaringSkjema -> Json.Encode.Value
encode (ValidertArbeidserfaringSkjema skjema) =
    [ [ ( "arbeidsgiver", Json.Encode.string skjema.bedriftNavn )
      , ( "yrke", Json.Encode.string (Yrke.label skjema.yrke) )
      , ( "sted", Json.Encode.string skjema.lokasjon )
      , ( "fradato", Dato.encodeMonthYear skjema.fraMåned skjema.fraÅr )
      , ( "yrkeFritekst", Json.Encode.string skjema.jobbTittel )
      , ( "beskrivelse", Json.Encode.string skjema.arbeidsoppgaver )
      , ( "styrkkode", (Yrke.styrkkode >> Json.Encode.string) skjema.yrke )
      , ( "konseptid", (Yrke.konseptId >> Json.Encode.int) skjema.yrke )
      ]
    , encodeTilDato skjema.tilDato
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
