module Skjema.Sertifikat exposing
    ( SertifikatSkjema
    , TypeaheadFelt(..)
    , Utløpsdato(..)
    , ValidertSertifikatSkjema
    , encode
    , feilmeldingFullførtÅr
    , feilmeldingUtløperÅr
    , fullførtMåned
    , fullførtÅr
    , fullførtÅrValidert
    , gjørAlleFeilmeldingerSynlig
    , id
    , initValidertSkjema
    , mapTypeaheadState
    , oppdaterFullførtMåned
    , oppdaterFullførtÅr
    , oppdaterSertifikatFelt
    , oppdaterUtløperMåned
    , oppdaterUtløperÅr
    , oppdaterUtsteder
    , sertifikatFeltValidert
    , sertifikatTypeahed
    , setSertifikatFelt
    , tilUvalidertSkjema
    , toggleUtløperIkke
    , utløperIkke
    , utløperMåned
    , utløperÅr
    , utløpsdatoValidert
    , utsteder
    , valider
    , velgAktivtSertifikatITypeahead
    )

import Dato exposing (Måned(..), År)
import Json.Encode
import SertifikatTypeahead exposing (SertifikatTypeahead)
import TypeaheadState exposing (TypeaheadState)


type SertifikatSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type ValidertSertifikatSkjema
    = ValidertSkjema ValidertSkjemaInfo


type Utløpsdato
    = IkkeOppgitt
    | Oppgitt Måned År


type TypeaheadFelt
    = SuggestionValgt SertifikatTypeahead
    | SuggestionIkkeValgt (TypeaheadState SertifikatTypeahead)


type alias UvalidertSkjemaInfo =
    { sertifikatFelt : TypeaheadFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : String
    , visFullførtÅrFeilmelding : Bool
    , utløperIkke : Bool
    , utløperMåned : Måned
    , utløperÅr : String
    , visUtløperÅrFeilmelding : Bool
    , id : Maybe String
    }


type alias ValidertSkjemaInfo =
    { sertifikatFelt : SertifikatTypeahead
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    , utløpsdato : Utløpsdato
    , id : Maybe String
    }



--INIT --


initValidertSkjema : ValidertSkjemaInfo -> ValidertSertifikatSkjema
initValidertSkjema skjema =
    ValidertSkjema skjema



--- INNHOLD ---


utsteder : SertifikatSkjema -> String
utsteder (UvalidertSkjema skjema) =
    skjema.utsteder


sertifikatTypeahed : SertifikatSkjema -> TypeaheadFelt
sertifikatTypeahed (UvalidertSkjema skjema) =
    skjema.sertifikatFelt


sertifikatFeltValidert : ValidertSertifikatSkjema -> SertifikatTypeahead
sertifikatFeltValidert (ValidertSkjema validert) =
    validert.sertifikatFelt


fullførtMåned : SertifikatSkjema -> Måned
fullførtMåned (UvalidertSkjema skjema) =
    skjema.fullførtMåned


fullførtÅrValidert : ValidertSertifikatSkjema -> År
fullførtÅrValidert (ValidertSkjema validert) =
    validert.fullførtÅr


fullførtÅr : SertifikatSkjema -> String
fullførtÅr (UvalidertSkjema skjema) =
    skjema.fullførtÅr


utløperIkke : SertifikatSkjema -> Bool
utløperIkke (UvalidertSkjema skjema) =
    skjema.utløperIkke


utløperMåned : SertifikatSkjema -> Måned
utløperMåned (UvalidertSkjema skjema) =
    skjema.utløperMåned


utløperÅr : SertifikatSkjema -> String
utløperÅr (UvalidertSkjema skjema) =
    skjema.utløperÅr


utløpsdatoValidert : ValidertSertifikatSkjema -> Utløpsdato
utløpsdatoValidert (ValidertSkjema validert) =
    validert.utløpsdato


id : ValidertSertifikatSkjema -> Maybe String
id (ValidertSkjema validert) =
    validert.id



--- OPPDATERING ---


oppdaterSertifikatFelt : SertifikatSkjema -> String -> SertifikatSkjema
oppdaterSertifikatFelt (UvalidertSkjema skjema) feltInnhold =
    case skjema.sertifikatFelt of
        SuggestionValgt _ ->
            UvalidertSkjema
                { skjema
                    | sertifikatFelt =
                        TypeaheadState.init feltInnhold
                            |> SuggestionIkkeValgt
                }

        SuggestionIkkeValgt typeaheadState ->
            UvalidertSkjema
                { skjema
                    | sertifikatFelt =
                        typeaheadState
                            |> TypeaheadState.updateValue feltInnhold
                            |> SuggestionIkkeValgt
                }


setSertifikatFelt : SertifikatTypeahead -> SertifikatSkjema -> SertifikatSkjema
setSertifikatFelt typeahaedSugestion (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | sertifikatFelt = SuggestionValgt typeahaedSugestion }


mapTypeaheadState : SertifikatSkjema -> (TypeaheadState SertifikatTypeahead -> TypeaheadState SertifikatTypeahead) -> SertifikatSkjema
mapTypeaheadState (UvalidertSkjema skjema) funksjon =
    case skjema.sertifikatFelt of
        SuggestionValgt _ ->
            UvalidertSkjema skjema

        SuggestionIkkeValgt typeaheadState ->
            UvalidertSkjema
                { skjema
                    | sertifikatFelt =
                        typeaheadState
                            |> funksjon
                            |> SuggestionIkkeValgt
                }


velgAktivtSertifikatITypeahead : SertifikatSkjema -> SertifikatSkjema
velgAktivtSertifikatITypeahead (UvalidertSkjema skjema) =
    case skjema.sertifikatFelt of
        SuggestionValgt _ ->
            UvalidertSkjema skjema

        SuggestionIkkeValgt typeaheadState ->
            case TypeaheadState.getActive typeaheadState of
                Just active ->
                    UvalidertSkjema { skjema | sertifikatFelt = SuggestionValgt active }

                Nothing ->
                    UvalidertSkjema skjema


oppdaterUtsteder : SertifikatSkjema -> String -> SertifikatSkjema
oppdaterUtsteder (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | utsteder = oppdatering }


oppdaterFullførtMåned : SertifikatSkjema -> Måned -> SertifikatSkjema
oppdaterFullførtMåned (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | fullførtMåned = oppdatering }


oppdaterFullførtÅr : SertifikatSkjema -> String -> SertifikatSkjema
oppdaterFullførtÅr (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | fullførtÅr = oppdatering }


oppdaterUtløperMåned : SertifikatSkjema -> Måned -> SertifikatSkjema
oppdaterUtløperMåned (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | utløperMåned = oppdatering }


oppdaterUtløperÅr : SertifikatSkjema -> String -> SertifikatSkjema
oppdaterUtløperÅr (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | utløperÅr = oppdatering }


toggleUtløperIkke : SertifikatSkjema -> SertifikatSkjema
toggleUtløperIkke (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | utløperIkke = not skjema.utløperIkke }



--- FEILMELDINGER ---


feilmeldingFullførtÅr : SertifikatSkjema -> Maybe String
feilmeldingFullførtÅr (UvalidertSkjema skjema) =
    if skjema.visFullførtÅrFeilmelding then
        Dato.feilmeldingÅr skjema.fullførtÅr

    else
        Nothing


feilmeldingUtløperÅr : SertifikatSkjema -> Maybe String
feilmeldingUtløperÅr (UvalidertSkjema skjema) =
    if not skjema.utløperIkke && skjema.visUtløperÅrFeilmelding then
        Dato.feilmeldingÅr skjema.utløperÅr

    else
        Nothing


gjørFeilmeldingFullførtÅrSynlig : SertifikatSkjema -> SertifikatSkjema
gjørFeilmeldingFullførtÅrSynlig (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visFullførtÅrFeilmelding = True }


gjørFeilmeldingUtløperÅrSynlig : SertifikatSkjema -> SertifikatSkjema
gjørFeilmeldingUtløperÅrSynlig (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visUtløperÅrFeilmelding = True }


gjørAlleFeilmeldingerSynlig : SertifikatSkjema -> SertifikatSkjema
gjørAlleFeilmeldingerSynlig skjema =
    skjema
        |> gjørFeilmeldingFullførtÅrSynlig
        |> gjørFeilmeldingUtløperÅrSynlig



--- VALIDERING ---


valider : SertifikatSkjema -> Maybe ValidertSertifikatSkjema
valider (UvalidertSkjema uvalidert) =
    case uvalidert.sertifikatFelt of
        SuggestionValgt sertifikatfelt ->
            if SertifikatTypeahead.label sertifikatfelt /= "" then
                Maybe.map2
                    (\utlopsdato fullføtÅr_ ->
                        ValidertSkjema
                            { sertifikatFelt = sertifikatfelt
                            , utsteder = uvalidert.utsteder
                            , fullførtMåned = uvalidert.fullførtMåned
                            , fullførtÅr = fullføtÅr_
                            , utløpsdato = utlopsdato
                            , id = uvalidert.id
                            }
                    )
                    (validerUtløpsdato uvalidert.utløperIkke uvalidert.utløperMåned uvalidert.utløperÅr)
                    (Dato.stringTilÅr uvalidert.fullførtÅr)

            else
                Nothing

        SuggestionIkkeValgt _ ->
            Nothing


validerUtløpsdato : Bool -> Måned -> String -> Maybe Utløpsdato
validerUtløpsdato utløperIkke_ måned år =
    if utløperIkke_ then
        Just IkkeOppgitt

    else
        år
            |> Dato.stringTilÅr
            |> Maybe.map (Oppgitt måned)


tilUvalidertSkjema : ValidertSertifikatSkjema -> SertifikatSkjema
tilUvalidertSkjema (ValidertSkjema validert) =
    UvalidertSkjema
        { sertifikatFelt = SuggestionValgt validert.sertifikatFelt
        , utsteder = validert.utsteder
        , fullførtMåned = validert.fullførtMåned
        , fullførtÅr = Dato.årTilString validert.fullførtÅr
        , visFullførtÅrFeilmelding = False
        , utløperMåned =
            case validert.utløpsdato of
                IkkeOppgitt ->
                    Januar

                Oppgitt måned _ ->
                    måned
        , utløperIkke = validert.utløpsdato == IkkeOppgitt
        , utløperÅr =
            case validert.utløpsdato of
                IkkeOppgitt ->
                    ""

                Oppgitt _ år ->
                    Dato.årTilString år
        , visUtløperÅrFeilmelding = False
        , id = validert.id
        }



-- ENCODE --


encode : ValidertSertifikatSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "fradato", Dato.encodeMonthYear skjema.fullførtMåned skjema.fullførtÅr )
      , ( "tildato", encodeTilDato skjema.utløpsdato )
      , ( "utsteder", Json.Encode.string skjema.utsteder )
      , ( "sertifikatnavn", (SertifikatTypeahead.label >> Json.Encode.string) skjema.sertifikatFelt )
      , ( "konseptId", (SertifikatTypeahead.konseptId >> Json.Encode.int) skjema.sertifikatFelt )
      , ( "sertifikatnavnFritekst", Json.Encode.null )
      ]
    ]
        |> List.concat
        |> Json.Encode.object


encodeTilDato : Utløpsdato -> Json.Encode.Value
encodeTilDato utløpsdagto =
    case utløpsdagto of
        IkkeOppgitt ->
            Json.Encode.null

        Oppgitt måned år ->
            Dato.encodeMonthYear måned år
