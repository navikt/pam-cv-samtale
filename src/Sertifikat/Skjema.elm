module Sertifikat.Skjema exposing
    ( SertifikatFelt(..)
    , SertifikatSkjema
    , Utløpsdato(..)
    , ValidertSertifikatSkjema
    , encode
    , feilmeldingFullførtÅr
    , feilmeldingSertifikatFelt
    , feilmeldingUtløperÅr
    , fullførtMåned
    , fullførtÅr
    , fullførtÅrValidert
    , id
    , initValidertSkjema
    , oppdaterFullførtMåned
    , oppdaterFullførtÅr
    , oppdaterSertifikat
    , oppdaterUtløperMåned
    , oppdaterUtløperÅr
    , oppdaterUtsteder
    , sertifikatFeltValidert
    , sertifikatString
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
    , visFeilmeldingSertifikatFelt
    , visFeilmeldingUtløperÅr
    )

import Dato exposing (Måned(..), År)
import Json.Encode
import SertifikatTypeahead exposing (SertifikatTypeahead)


type SertifikatSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type ValidertSertifikatSkjema
    = ValidertSkjema ValidertSkjemaInfo


type Utløpsdato
    = IkkeOppgitt
    | Oppgitt Måned År


type SertifikatFelt
    = SertifikatFraTypeahead SertifikatTypeahead
    | Egendefinert String


type alias UvalidertSkjemaInfo =
    { sertifikatFelt : SertifikatFelt
    , visSertifikatFeltFeilmelding : Bool
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
    { sertifikatFelt : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    , utløpsdato : Utløpsdato
    , id : Maybe String
    }



--- INIT ---


initValidertSkjema : ValidertSkjemaInfo -> ValidertSertifikatSkjema
initValidertSkjema skjema =
    ValidertSkjema skjema



--- INNHOLD ---


utsteder : SertifikatSkjema -> String
utsteder (UvalidertSkjema skjema) =
    skjema.utsteder


sertifikatFeltValidert : ValidertSertifikatSkjema -> SertifikatFelt
sertifikatFeltValidert (ValidertSkjema validert) =
    validert.sertifikatFelt


sertifikatString : ValidertSertifikatSkjema -> String
sertifikatString (ValidertSkjema validert) =
    case validert.sertifikatFelt of
        SertifikatFraTypeahead sertifikatTypeahead ->
            SertifikatTypeahead.label sertifikatTypeahead

        Egendefinert inputValue ->
            inputValue


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


oppdaterSertifikat : SertifikatSkjema -> SertifikatFelt -> SertifikatSkjema
oppdaterSertifikat (UvalidertSkjema skjema) sertifikat =
    UvalidertSkjema { skjema | sertifikatFelt = sertifikat }


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


feilmeldingSertifikatFelt : SertifikatSkjema -> Maybe String
feilmeldingSertifikatFelt (UvalidertSkjema skjema) =
    if skjema.visSertifikatFeltFeilmelding then
        case validerSertifikatFelt skjema.sertifikatFelt of
            Just _ ->
                Nothing

            Nothing ->
                Just "Velg eller skriv inn sertifisering eller sertifikat"

    else
        Nothing


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


visFeilmeldingSertifikatFelt : SertifikatSkjema -> SertifikatSkjema
visFeilmeldingSertifikatFelt (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visSertifikatFeltFeilmelding = True }


visFeilmeldingFullførtÅr : SertifikatSkjema -> SertifikatSkjema
visFeilmeldingFullførtÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visFullførtÅrFeilmelding = True }


visFeilmeldingUtløperÅr : SertifikatSkjema -> SertifikatSkjema
visFeilmeldingUtløperÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visUtløperÅrFeilmelding = True }


visAlleFeilmeldinger : SertifikatSkjema -> SertifikatSkjema
visAlleFeilmeldinger skjema =
    skjema
        |> visFeilmeldingFullførtÅr
        |> visFeilmeldingUtløperÅr
        |> visFeilmeldingSertifikatFelt



--- VALIDERING ---


valider : SertifikatSkjema -> Maybe ValidertSertifikatSkjema
valider (UvalidertSkjema uvalidert) =
    Maybe.map3
        (\sertifikatFelt utlopsdato fullføtÅr_ ->
            ValidertSkjema
                { sertifikatFelt = sertifikatFelt
                , utsteder = uvalidert.utsteder
                , fullførtMåned = uvalidert.fullførtMåned
                , fullførtÅr = fullføtÅr_
                , utløpsdato = utlopsdato
                , id = uvalidert.id
                }
        )
        (validerSertifikatFelt uvalidert.sertifikatFelt)
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


validerSertifikatFelt : SertifikatFelt -> Maybe SertifikatFelt
validerSertifikatFelt sertifikatFelt =
    case sertifikatFelt of
        SertifikatFraTypeahead typeaheadSertifikat ->
            Just (SertifikatFraTypeahead typeaheadSertifikat)

        Egendefinert inputString ->
            if String.length (String.trim inputString) == 0 then
                Nothing

            else
                Just (Egendefinert inputString)


tilUvalidertSkjema : ValidertSertifikatSkjema -> SertifikatSkjema
tilUvalidertSkjema (ValidertSkjema validert) =
    UvalidertSkjema
        { sertifikatFelt = validert.sertifikatFelt
        , visSertifikatFeltFeilmelding = False
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
      , ( "sertifikatnavnFritekst", Json.Encode.null )
      ]
    , encodeSertifikatFelt skjema.sertifikatFelt
    ]
        |> List.concat
        |> Json.Encode.object


encodeSertifikatFelt : SertifikatFelt -> List ( String, Json.Encode.Value )
encodeSertifikatFelt sertifikatFelt =
    case sertifikatFelt of
        SertifikatFraTypeahead sertifikatTypeahead ->
            [ ( "sertifikatnavn", (SertifikatTypeahead.label >> Json.Encode.string) sertifikatTypeahead )
            , ( "konseptId", (SertifikatTypeahead.konseptId >> Json.Encode.int) sertifikatTypeahead )
            ]

        Egendefinert string ->
            [ ( "sertifikatnavn", Json.Encode.string string ) ]


encodeTilDato : Utløpsdato -> Json.Encode.Value
encodeTilDato utløpsdagto =
    case utløpsdagto of
        IkkeOppgitt ->
            Json.Encode.null

        Oppgitt måned år ->
            Dato.encodeMonthYear måned år
