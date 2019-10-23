module AnnenErfaring.Skjema exposing
    ( AnnenErfaringSkjema
    , Felt(..)
    , ValidertAnnenErfaringSkjema
    , encode
    , feilmeldingBeskrivelse
    , feilmeldingFraÅr
    , feilmeldingRolle
    , feilmeldingTilÅr
    , fraMåned
    , fraÅrValidert
    , id
    , initValidertSkjema
    , innholdTekstFelt
    , nåværende
    , oppdaterFraMåned
    , oppdaterTekstFelt
    , oppdaterTilMåned
    , tilDatoValidert
    , tilMåned
    , tilUvalidertSkjema
    , toggleNavarende
    , valider
    , visAlleFeilmeldinger
    , visFeilmeldingFraÅr
    , visFeilmeldingTilÅr
    )

import Dato exposing (Måned(..), TilDato(..), År)
import Json.Encode


type AnnenErfaringSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type ValidertAnnenErfaringSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias UvalidertSkjemaInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : String
    , nåværende : Bool
    , tilMåned : Måned
    , tilÅr : String
    , id : Maybe String
    , visFeilmeldingRolle : Bool
    , visFeilmeldingFraÅr : Bool
    , visFeilmeldingTilÅr : Bool
    }


type alias ValidertSkjemaInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    , tilDato : TilDato
    , id : Maybe String
    }



--- INIT ---


initValidertSkjema : ValidertSkjemaInfo -> ValidertAnnenErfaringSkjema
initValidertSkjema info =
    ValidertSkjema info



--- INNHOLD ---


type Felt
    = Rolle
    | Beskrivelse
    | FraÅr
    | TilÅr


innholdTekstFelt : Felt -> AnnenErfaringSkjema -> String
innholdTekstFelt felt (UvalidertSkjema skjema) =
    case felt of
        FraÅr ->
            skjema.fraÅr

        TilÅr ->
            skjema.tilÅr

        Rolle ->
            skjema.rolle

        Beskrivelse ->
            skjema.beskrivelse


nåværende : AnnenErfaringSkjema -> Bool
nåværende (UvalidertSkjema info) =
    info.nåværende


fraMåned : AnnenErfaringSkjema -> Måned
fraMåned (UvalidertSkjema info) =
    info.fraMåned


tilMåned : AnnenErfaringSkjema -> Måned
tilMåned (UvalidertSkjema info) =
    info.tilMåned


fraÅrValidert : ValidertAnnenErfaringSkjema -> År
fraÅrValidert (ValidertSkjema info) =
    info.fraÅr


tilDatoValidert : ValidertAnnenErfaringSkjema -> TilDato
tilDatoValidert (ValidertSkjema info) =
    info.tilDato


id : ValidertAnnenErfaringSkjema -> Maybe String
id (ValidertSkjema info) =
    info.id



--- OPPDATERING ---


oppdaterTekstFelt : Felt -> String -> AnnenErfaringSkjema -> AnnenErfaringSkjema
oppdaterTekstFelt felt tekst (UvalidertSkjema skjema) =
    case felt of
        Rolle ->
            UvalidertSkjema { skjema | rolle = tekst }

        Beskrivelse ->
            UvalidertSkjema { skjema | beskrivelse = tekst }

        FraÅr ->
            UvalidertSkjema { skjema | fraÅr = tekst }

        TilÅr ->
            UvalidertSkjema { skjema | tilÅr = tekst }


toggleNavarende : AnnenErfaringSkjema -> AnnenErfaringSkjema
toggleNavarende (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | nåværende = not skjema.nåværende }


oppdaterFraMåned : AnnenErfaringSkjema -> Måned -> AnnenErfaringSkjema
oppdaterFraMåned (UvalidertSkjema skjema) måned =
    UvalidertSkjema { skjema | fraMåned = måned }


oppdaterTilMåned : AnnenErfaringSkjema -> Måned -> AnnenErfaringSkjema
oppdaterTilMåned (UvalidertSkjema skjema) måned =
    UvalidertSkjema { skjema | tilMåned = måned }



--- FEILMELDINGER ---


feilmeldingRolle : String -> Maybe String
feilmeldingRolle rolle_ =
    if String.length (String.trim rolle_) == 0 then
        Just "Vennligst fyll inn en rolle"

    else if String.length rolle_ > 250 then
        let
            tallTekst =
                (String.length rolle_ - 250)
                    |> String.fromInt
        in
        Just ("Du har " ++ tallTekst ++ " tegn for mye")

    else
        Nothing


feilmeldingBeskrivelse : String -> Maybe String
feilmeldingBeskrivelse innhold =
    if String.length innhold <= 2000 then
        Nothing

    else
        let
            tallTekst =
                (String.length innhold - 2000)
                    |> String.fromInt
        in
        Just ("Du har " ++ tallTekst ++ " tegn for mye")


feilmeldingFraÅr : AnnenErfaringSkjema -> Maybe String
feilmeldingFraÅr (UvalidertSkjema skjema) =
    if skjema.visFeilmeldingFraÅr then
        Dato.feilmeldingÅr skjema.fraÅr

    else
        Nothing


feilmeldingTilÅr : AnnenErfaringSkjema -> Maybe String
feilmeldingTilÅr (UvalidertSkjema skjema) =
    if not skjema.nåværende && skjema.visFeilmeldingTilÅr then
        Dato.feilmeldingÅr skjema.tilÅr

    else
        Nothing


visFeilmeldingFraÅr : AnnenErfaringSkjema -> AnnenErfaringSkjema
visFeilmeldingFraÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visFeilmeldingFraÅr = True }


visFeilmeldingTilÅr : AnnenErfaringSkjema -> AnnenErfaringSkjema
visFeilmeldingTilÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visFeilmeldingTilÅr = True }


visFeilmeldingRolle : AnnenErfaringSkjema -> AnnenErfaringSkjema
visFeilmeldingRolle (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | visFeilmeldingRolle = True }


visAlleFeilmeldinger : AnnenErfaringSkjema -> AnnenErfaringSkjema
visAlleFeilmeldinger skjema =
    skjema
        |> visFeilmeldingFraÅr
        |> visFeilmeldingTilÅr
        |> visFeilmeldingRolle



--- VALIDERING ---


valider : AnnenErfaringSkjema -> Maybe ValidertAnnenErfaringSkjema
valider (UvalidertSkjema uvalidert) =
    if feilmeldingRolle uvalidert.rolle /= Nothing then
        Nothing

    else if feilmeldingBeskrivelse uvalidert.beskrivelse /= Nothing then
        Nothing

    else
        Maybe.map2
            (\fraÅr_ tilDato_ ->
                ValidertSkjema
                    { rolle = uvalidert.rolle
                    , beskrivelse = uvalidert.beskrivelse
                    , fraMåned = uvalidert.fraMåned
                    , fraÅr = fraÅr_
                    , tilDato = tilDato_
                    , id = uvalidert.id
                    }
            )
            (Dato.stringTilÅr uvalidert.fraÅr)
            (validerTilDato uvalidert.nåværende uvalidert.tilMåned uvalidert.tilÅr)


validerTilDato : Bool -> Måned -> String -> Maybe TilDato
validerTilDato nåværende_ måned år =
    if nåværende_ then
        Just Nåværende

    else
        år
            |> Dato.stringTilÅr
            |> Maybe.map (Avsluttet måned)


tilDatoMåned : TilDato -> Måned
tilDatoMåned tilDato =
    case tilDato of
        Nåværende ->
            Januar

        Avsluttet måned _ ->
            måned


tilDatoÅr : TilDato -> String
tilDatoÅr tilDato =
    case tilDato of
        Nåværende ->
            ""

        Avsluttet _ år ->
            Dato.årTilString år


tilUvalidertSkjema : ValidertAnnenErfaringSkjema -> AnnenErfaringSkjema
tilUvalidertSkjema (ValidertSkjema validert) =
    UvalidertSkjema
        { rolle = validert.rolle
        , beskrivelse = validert.beskrivelse
        , fraMåned = validert.fraMåned
        , fraÅr = Dato.årTilString validert.fraÅr
        , nåværende = validert.tilDato == Nåværende
        , tilMåned = tilDatoMåned validert.tilDato
        , tilÅr = tilDatoÅr validert.tilDato
        , visFeilmeldingFraÅr = False
        , visFeilmeldingTilÅr = False
        , visFeilmeldingRolle = False
        , id = validert.id
        }



-- ENCODE --


encode : ValidertAnnenErfaringSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "rolle", Json.Encode.string skjema.rolle )
      , ( "beskrivelse", Json.Encode.string skjema.beskrivelse )
      , ( "fradato", Dato.encodeMonthYear skjema.fraMåned skjema.fraÅr )
      ]
    , encodeTilDato skjema.tilDato
    ]
        |> List.concat
        |> Json.Encode.object


encodeTilDato : TilDato -> List ( String, Json.Encode.Value )
encodeTilDato tilDato =
    case tilDato of
        Nåværende ->
            [ ( "naavaerende", Json.Encode.bool True ) ]

        Avsluttet måned år ->
            [ ( "naavaerende", Json.Encode.bool False )
            , ( "tildato", Dato.encodeMonthYear måned år )
            ]
