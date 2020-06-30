module Godkjenning.Skjema exposing
    ( GodkjenningFelt(..)
    , GodkjenningSkjema
    , Utløpsdato(..)
    , ValidertGodkjenningSkjema
    , encode
    , feilmeldingFullførtÅr
    , feilmeldingGodkjenningFelt
    , feilmeldingUtløperÅr
    , fullførtMåned
    , fullførtÅr
    , fullførtÅrValidert
    , godkjenningFeltValidert
    , godkjenningString
    , id
    , initValidertSkjema
    , oppdaterFullførtMåned
    , oppdaterFullførtÅr
    , oppdaterGodkjenning
    , oppdaterUtløperMåned
    , oppdaterUtløperÅr
    , oppdaterUtsteder
    , tilUvalidertSkjema
    , toggleUtløperIkke
    , utløperIkke
    , utløperMåned
    , utløperÅr
    , utløpsdatoValidert
    , utsteder
    , valider
    , visAlleFeilmeldinger
    , visFeilmeldingFullførtÅr
    , visFeilmeldingGodkjenningFelt
    , visFeilmeldingUtløperÅr
    )

import Dato.Dato as Dato exposing (År)
import Dato.Maned exposing (Måned(..))
import Godkjenning.GodkjenningTypeahead as GodkjenningTypeahead exposing (GodkjenningTypeahead)
import Json.Encode


type GodkjenningSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type ValidertGodkjenningSkjema
    = ValidertSkjema ValidertSkjemaInfo


type Utløpsdato
    = IkkeOppgitt
    | Oppgitt Måned År


type GodkjenningFelt
    = GodkjenningFraTypeahead GodkjenningTypeahead
    | Egendefinert String


type alias UvalidertSkjemaInfo =
    { godkjenningFelt : GodkjenningFelt
    , visGodkjenningFeltFeilmelding : Bool
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
    { godkjenningFelt : GodkjenningFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    , utløpsdato : Utløpsdato
    , id : Maybe String
    }



--- INIT ---


initValidertSkjema : ValidertSkjemaInfo -> ValidertGodkjenningSkjema
initValidertSkjema skjema =
    ValidertSkjema skjema



--- INNHOLD ---


utsteder : GodkjenningSkjema -> String
utsteder (UvalidertSkjema skjema) =
    skjema.utsteder


godkjenningFeltValidert : ValidertGodkjenningSkjema -> GodkjenningFelt
godkjenningFeltValidert (ValidertSkjema validert) =
    validert.godkjenningFelt


godkjenningString : ValidertGodkjenningSkjema -> String
godkjenningString (ValidertSkjema validert) =
    case validert.godkjenningFelt of
        GodkjenningFraTypeahead godkjenningTypeahead ->
            GodkjenningTypeahead.label godkjenningTypeahead

        Egendefinert inputValue ->
            inputValue


fullførtMåned : GodkjenningSkjema -> Måned
fullførtMåned (UvalidertSkjema skjema) =
    skjema.fullførtMåned


fullførtÅrValidert : ValidertGodkjenningSkjema -> År
fullførtÅrValidert (ValidertSkjema validert) =
    validert.fullførtÅr


fullførtÅr : GodkjenningSkjema -> String
fullførtÅr (UvalidertSkjema skjema) =
    skjema.fullførtÅr


utløperIkke : GodkjenningSkjema -> Bool
utløperIkke (UvalidertSkjema skjema) =
    skjema.utløperIkke


utløperMåned : GodkjenningSkjema -> Måned
utløperMåned (UvalidertSkjema skjema) =
    skjema.utløperMåned


utløperÅr : GodkjenningSkjema -> String
utløperÅr (UvalidertSkjema skjema) =
    skjema.utløperÅr


utløpsdatoValidert : ValidertGodkjenningSkjema -> Utløpsdato
utløpsdatoValidert (ValidertSkjema validert) =
    validert.utløpsdato


id : ValidertGodkjenningSkjema -> Maybe String
id (ValidertSkjema validert) =
    validert.id



--- OPPDATERING ---


oppdaterGodkjenning : GodkjenningSkjema -> GodkjenningFelt -> GodkjenningSkjema
oppdaterGodkjenning (UvalidertSkjema skjema) godkjenning =
    UvalidertSkjema { skjema | godkjenningFelt = godkjenning }


oppdaterUtsteder : GodkjenningSkjema -> String -> GodkjenningSkjema
oppdaterUtsteder (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | utsteder = oppdatering }


oppdaterFullførtMåned : GodkjenningSkjema -> Måned -> GodkjenningSkjema
oppdaterFullførtMåned (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | fullførtMåned = oppdatering }


oppdaterFullførtÅr : GodkjenningSkjema -> String -> GodkjenningSkjema
oppdaterFullførtÅr (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | fullførtÅr = oppdatering }


oppdaterUtløperMåned : GodkjenningSkjema -> Måned -> GodkjenningSkjema
oppdaterUtløperMåned (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | utløperMåned = oppdatering }


oppdaterUtløperÅr : GodkjenningSkjema -> String -> GodkjenningSkjema
oppdaterUtløperÅr (UvalidertSkjema skjema) oppdatering =
    UvalidertSkjema { skjema | utløperÅr = oppdatering }


toggleUtløperIkke : GodkjenningSkjema -> GodkjenningSkjema
toggleUtløperIkke (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | utløperIkke = not skjema.utløperIkke }



--- FEILMELDINGER ---


feilmeldingGodkjenningFelt : GodkjenningSkjema -> Maybe String
feilmeldingGodkjenningFelt (UvalidertSkjema skjema) =
    if skjema.visGodkjenningFeltFeilmelding then
        case validerGodkjenningFelt skjema.godkjenningFelt of
            Just _ ->
                Nothing

            Nothing ->
                Just "Velg en godkjenning fra listen med forslag som kommer opp"

    else
        Nothing


feilmeldingFullførtÅr : GodkjenningSkjema -> Maybe String
feilmeldingFullførtÅr (UvalidertSkjema skjema) =
    if skjema.visFullførtÅrFeilmelding then
        Dato.feilmeldingÅr skjema.fullførtÅr

    else
        Nothing


feilmeldingUtløperÅr : GodkjenningSkjema -> Maybe String
feilmeldingUtløperÅr (UvalidertSkjema skjema) =
    if not skjema.utløperIkke && skjema.visUtløperÅrFeilmelding then
        Dato.feilmeldingÅr skjema.utløperÅr

    else
        Nothing


visFeilmeldingGodkjenningFelt : Bool -> GodkjenningSkjema -> GodkjenningSkjema
visFeilmeldingGodkjenningFelt synlig (UvalidertSkjema skjema) =
    -- Skal alltid vises etter onBlur/onSubmit, så hvis den noen gang har vært True, skal den alltid fortsette å være True
    UvalidertSkjema { skjema | visGodkjenningFeltFeilmelding = synlig || skjema.visGodkjenningFeltFeilmelding }


visFeilmeldingFullførtÅr : GodkjenningSkjema -> GodkjenningSkjema
visFeilmeldingFullførtÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visFullførtÅrFeilmelding = True }


visFeilmeldingUtløperÅr : GodkjenningSkjema -> GodkjenningSkjema
visFeilmeldingUtløperÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visUtløperÅrFeilmelding = True }


visAlleFeilmeldinger : GodkjenningSkjema -> GodkjenningSkjema
visAlleFeilmeldinger skjema =
    skjema
        |> visFeilmeldingGodkjenningFelt True
        |> visFeilmeldingFullførtÅr
        |> visFeilmeldingUtløperÅr



--- VALIDERING ---


valider : GodkjenningSkjema -> Maybe ValidertGodkjenningSkjema
valider (UvalidertSkjema uvalidert) =
    Maybe.map3
        (\godkjenningFelt utlopsdato fullførtÅr_ ->
            ValidertSkjema
                { godkjenningFelt = godkjenningFelt
                , utsteder = uvalidert.utsteder
                , fullførtMåned = uvalidert.fullførtMåned
                , fullførtÅr = fullførtÅr_
                , utløpsdato = utlopsdato
                , id = uvalidert.id
                }
        )
        (validerGodkjenningFelt uvalidert.godkjenningFelt)
        (validerUtløpsdato uvalidert.utløperIkke uvalidert.utløperMåned uvalidert.utløperÅr)
        (Dato.stringTilÅr uvalidert.fullførtÅr)


validerUtløpsdato : Bool -> Måned -> String -> Maybe Utløpsdato
validerUtløpsdato utløperIkke_ måned år =
    if utløperIkke_ then
        Just IkkeOppgitt

    else
        år
            |> Dato.stringTilÅr
            |> Maybe.map (Oppgitt måned)


validerGodkjenningFelt : GodkjenningFelt -> Maybe GodkjenningFelt
validerGodkjenningFelt godkjenningFelt =
    case godkjenningFelt of
        GodkjenningFraTypeahead typeaheadGodkjenning ->
            Just (GodkjenningFraTypeahead typeaheadGodkjenning)

        Egendefinert inputString ->
            if String.length (String.trim inputString) == 0 then
                Nothing

            else
                Just (Egendefinert inputString)


tilUvalidertSkjema : ValidertGodkjenningSkjema -> GodkjenningSkjema
tilUvalidertSkjema (ValidertSkjema validert) =
    UvalidertSkjema
        { godkjenningFelt = validert.godkjenningFelt
        , visGodkjenningFeltFeilmelding = False
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



--- ENCODE ---


encode : ValidertGodkjenningSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "fradato", Dato.encodeMonthYear skjema.fullførtMåned skjema.fullførtÅr )
      , ( "tildato", encodeTilDato skjema.utløpsdato )
      , ( "utsteder", Json.Encode.string skjema.utsteder )
      ]
    , encodeGodkjenningFelt skjema.godkjenningFelt
    ]
        |> List.concat
        |> Json.Encode.object


encodeGodkjenningFelt : GodkjenningFelt -> List ( String, Json.Encode.Value )
encodeGodkjenningFelt godkjenningFelt =
    case godkjenningFelt of
        GodkjenningFraTypeahead godkjenningTypeahead ->
            [ ( "tittel", (GodkjenningTypeahead.label >> Json.Encode.string) godkjenningTypeahead )
            , ( "konseptId", (GodkjenningTypeahead.konseptId >> Json.Encode.int) godkjenningTypeahead )
            ]

        Egendefinert string ->
            [ ( "tittel", Json.Encode.string string ) ]


encodeTilDato : Utløpsdato -> Json.Encode.Value
encodeTilDato utløpsdato =
    case utløpsdato of
        IkkeOppgitt ->
            Json.Encode.null

        Oppgitt måned år ->
            Dato.encodeMonthYear måned år
