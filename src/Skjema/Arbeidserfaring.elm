module Skjema.Arbeidserfaring exposing
    ( ArbeidserfaringSkjema
    , Felt(..)
    , SkjemaInfo
    , TypeaheadFelt(..)
    , ValidertArbeidserfaringSkjema
    , encode
    , fraArbeidserfaring
    , fraMåned
    , fraÅrValidert
    , id
    , initValidertSkjema
    , innholdTekstFelt
    , mapTypeaheadState
    , nåværende
    , oppdaterFraMåned
    , oppdaterStringFelt
    , oppdaterTilMåned
    , oppdaterYrkeFelt
    , setYrkeFeltTilYrke
    , tilDatoValidert
    , tilMåned
    , tilUvalidertSkjema
    , toggleNåværende
    , valider
    , velgAktivYrkeITypeahead
    , yrke
    , yrkeTypeahead
    )

import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Dato exposing (Måned(..), TilDato(..), År)
import Json.Encode
import TypeaheadState exposing (TypeaheadState)
import Yrke exposing (Yrke)


type ArbeidserfaringSkjema
    = ArbeidserfaringSkjema SkjemaInfo


type alias SkjemaInfo =
    { yrke : TypeaheadFelt
    , jobbTittel : String
    , bedriftsnavn : String
    , sted : String
    , arbeidsoppgaver : String
    , fraMåned : Måned
    , fraÅr : String
    , nåværende : Bool
    , tilMåned : Måned
    , tilÅr : String
    , id : Maybe String
    }


type TypeaheadFelt
    = Yrke Yrke
    | Typeahead (TypeaheadState Yrke)



--- INIT ---


initValidertSkjema : ValidertSkjemaInfo -> ValidertArbeidserfaringSkjema
initValidertSkjema skjemaInfo =
    ValidertArbeidserfaringSkjema skjemaInfo


fraArbeidserfaring : Arbeidserfaring -> ArbeidserfaringSkjema
fraArbeidserfaring arbeidserfaring =
    ArbeidserfaringSkjema
        { yrke =
            arbeidserfaring
                |> Arbeidserfaring.yrke
                |> Maybe.map Yrke
                |> Maybe.withDefault (Typeahead (TypeaheadState.init ""))
        , jobbTittel = (Arbeidserfaring.yrkeFritekst >> Maybe.withDefault "") arbeidserfaring
        , bedriftsnavn = (Arbeidserfaring.arbeidsgiver >> Maybe.withDefault "") arbeidserfaring
        , sted = (Arbeidserfaring.sted >> Maybe.withDefault "") arbeidserfaring
        , arbeidsoppgaver = (Arbeidserfaring.beskrivelse >> Maybe.withDefault "") arbeidserfaring
        , fraMåned = Arbeidserfaring.fraMåned arbeidserfaring
        , fraÅr = (Arbeidserfaring.fraÅr >> Dato.årTilString) arbeidserfaring
        , nåværende = Arbeidserfaring.tilDato arbeidserfaring == Nåværende
        , tilMåned = (Arbeidserfaring.tilDato >> månedFraTilDato) arbeidserfaring
        , tilÅr = (Arbeidserfaring.tilDato >> årFraTilDato) arbeidserfaring
        , id = (Arbeidserfaring.id >> Just) arbeidserfaring
        }


tilUvalidertSkjema : ValidertArbeidserfaringSkjema -> ArbeidserfaringSkjema
tilUvalidertSkjema (ValidertArbeidserfaringSkjema info) =
    ArbeidserfaringSkjema
        { yrke = Yrke info.yrke
        , jobbTittel = info.jobbTittel
        , bedriftsnavn = info.bedriftNavn
        , sted = info.lokasjon
        , arbeidsoppgaver = info.arbeidsoppgaver
        , fraMåned = info.fraMåned
        , fraÅr = Dato.årTilString info.fraÅr
        , nåværende = info.tilDato == Nåværende
        , tilMåned = månedFraTilDato info.tilDato
        , tilÅr = årFraTilDato info.tilDato
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


yrkeTypeahead : ArbeidserfaringSkjema -> TypeaheadFelt
yrkeTypeahead (ArbeidserfaringSkjema info) =
    info.yrke


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


oppdaterYrkeFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterYrkeFelt (ArbeidserfaringSkjema skjema) string =
    case skjema.yrke of
        Yrke _ ->
            ArbeidserfaringSkjema
                { skjema
                    | yrke =
                        TypeaheadState.init string
                            |> Typeahead
                }

        Typeahead typeaheadState ->
            ArbeidserfaringSkjema
                { skjema
                    | yrke =
                        typeaheadState
                            |> TypeaheadState.updateValue string
                            |> Typeahead
                }


mapTypeaheadState : ArbeidserfaringSkjema -> (TypeaheadState Yrke -> TypeaheadState Yrke) -> ArbeidserfaringSkjema
mapTypeaheadState (ArbeidserfaringSkjema skjema) funksjon =
    case skjema.yrke of
        Yrke _ ->
            ArbeidserfaringSkjema
                skjema

        Typeahead typeaheadState ->
            ArbeidserfaringSkjema
                { skjema
                    | yrke =
                        typeaheadState
                            |> funksjon
                            |> Typeahead
                }


velgAktivYrkeITypeahead : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
velgAktivYrkeITypeahead (ArbeidserfaringSkjema info) =
    case info.yrke of
        Yrke _ ->
            ArbeidserfaringSkjema info

        Typeahead typeaheadState ->
            case TypeaheadState.getActive typeaheadState of
                Just active ->
                    ArbeidserfaringSkjema { info | yrke = Yrke active }

                Nothing ->
                    ArbeidserfaringSkjema info


setYrkeFeltTilYrke : Yrke -> ArbeidserfaringSkjema -> ArbeidserfaringSkjema
setYrkeFeltTilYrke yrke_ (ArbeidserfaringSkjema info) =
    ArbeidserfaringSkjema { info | yrke = Yrke yrke_ }



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
    case info.yrke of
        Yrke yrkefelt ->
            if Yrke.label yrkefelt /= "" then
                Maybe.map2
                    (\tilDato fraÅr_ ->
                        ValidertArbeidserfaringSkjema
                            { yrke = yrkefelt
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
                    (validerTilDato info.nåværende info.tilMåned info.tilÅr)
                    (Dato.stringTilÅr info.fraÅr)

            else
                Nothing

        Typeahead _ ->
            Nothing


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
