module AnnenErfaring.Skjema exposing
    ( AnnenErfaringSkjema
    , Felt(..)
    , ValidertAnnenErfaringSkjema
    , encode
    , feilmeldingFraMåned
    , feilmeldingFraÅr
    , feilmeldingRolle
    , feilmeldingRolleHvisSynlig
    , feilmeldingTilMåned
    , feilmeldingTilÅr
    , fraMåned
    , harDatoer
    , id
    , initValidertSkjema
    , initValidertSkjemaUtenPeriode
    , innholdTekstFelt
    , nåværende
    , oppdaterFraMåned
    , oppdaterTekstFelt
    , oppdaterTilMåned
    , periode
    , tilMåned
    , tilUvalidertSkjema
    , tillatÅViseAlleFeilmeldinger
    , tillatÅViseFeilmeldingFraÅr
    , tillatÅViseFeilmeldingMåned
    , tillatÅViseFeilmeldingRolle
    , tillatÅViseFeilmeldingTilÅr
    , toggleHarDatoer
    , toggleNavarende
    , valider
    )

import Dato.Dato as Dato exposing (DatoPeriode(..), TilDato(..), År)
import Dato.Maned exposing (Måned(..))
import Json.Encode
import Validering


type AnnenErfaringSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type ValidertAnnenErfaringSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias UvalidertSkjemaInfo =
    { rolle : String
    , beskrivelse : String
    , harDatoer : Bool
    , fraMåned : Maybe Måned
    , fraÅr : String
    , nåværende : Bool
    , tilMåned : Maybe Måned
    , tilÅr : String
    , id : Maybe String
    , tillatÅViseFeilmeldingRolle : Bool
    , tillatÅViseFeilmeldingFraÅr : Bool
    , tillatÅViseFeilmeldingTilÅr : Bool
    , tillatÅViseFeilmeldingPeriode : Bool
    }


type alias ValidertSkjemaInfo =
    { rolle : String
    , beskrivelse : String
    , periode : DatoPeriode
    , id : Maybe String
    }



--- INIT ---


type alias ValidertSkjemaInitMedPeriode =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    , tilDato : TilDato
    , id : Maybe String
    }


initValidertSkjema : ValidertSkjemaInitMedPeriode -> ValidertAnnenErfaringSkjema
initValidertSkjema info =
    ValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , periode = Oppgitt info.fraMåned info.fraÅr info.tilDato
        , id = info.id
        }


type alias ValidertSkjemaInitUtenPeriode =
    { rolle : String
    , beskrivelse : String
    , id : Maybe String
    }


initValidertSkjemaUtenPeriode : ValidertSkjemaInitUtenPeriode -> ValidertAnnenErfaringSkjema
initValidertSkjemaUtenPeriode info =
    ValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , periode = IkkeOppgitt
        , id = info.id
        }



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


harDatoer : AnnenErfaringSkjema -> Bool
harDatoer (UvalidertSkjema info) =
    info.harDatoer


fraMåned : AnnenErfaringSkjema -> Maybe Måned
fraMåned (UvalidertSkjema info) =
    info.fraMåned


tilMåned : AnnenErfaringSkjema -> Maybe Måned
tilMåned (UvalidertSkjema info) =
    info.tilMåned


periode : ValidertAnnenErfaringSkjema -> DatoPeriode
periode (ValidertSkjema skjema) =
    skjema.periode


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


oppdaterFraMåned : AnnenErfaringSkjema -> Maybe Måned -> AnnenErfaringSkjema
oppdaterFraMåned (UvalidertSkjema skjema) måned =
    UvalidertSkjema { skjema | fraMåned = måned }


oppdaterTilMåned : AnnenErfaringSkjema -> Maybe Måned -> AnnenErfaringSkjema
oppdaterTilMåned (UvalidertSkjema skjema) måned =
    UvalidertSkjema { skjema | tilMåned = måned }


toggleHarDatoer : AnnenErfaringSkjema -> AnnenErfaringSkjema
toggleHarDatoer (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | harDatoer = not skjema.harDatoer }



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


feilmeldingRolleHvisSynlig : AnnenErfaringSkjema -> Maybe String
feilmeldingRolleHvisSynlig (UvalidertSkjema skjema) =
    if skjema.tillatÅViseFeilmeldingRolle then
        feilmeldingRolle skjema.rolle

    else
        Nothing


feilmeldingFraMåned : AnnenErfaringSkjema -> Maybe String
feilmeldingFraMåned (UvalidertSkjema skjema) =
    if skjema.harDatoer && skjema.tillatÅViseFeilmeldingPeriode then
        Dato.feilmeldingValgfriMåned skjema.fraMåned

    else
        Nothing


feilmeldingTilMåned : AnnenErfaringSkjema -> Maybe String
feilmeldingTilMåned (UvalidertSkjema skjema) =
    if skjema.harDatoer && not skjema.nåværende && skjema.tillatÅViseFeilmeldingPeriode then
        Dato.feilmeldingValgfriMåned skjema.tilMåned

    else
        Nothing


feilmeldingFraÅr : AnnenErfaringSkjema -> Maybe String
feilmeldingFraÅr (UvalidertSkjema skjema) =
    if skjema.tillatÅViseFeilmeldingFraÅr then
        Dato.feilmeldingÅr skjema.fraÅr

    else
        Nothing


feilmeldingTilÅr : AnnenErfaringSkjema -> Maybe String
feilmeldingTilÅr (UvalidertSkjema skjema) =
    if not skjema.nåværende && skjema.tillatÅViseFeilmeldingTilÅr then
        Dato.feilmeldingÅr skjema.tilÅr

    else
        Nothing


tillatÅViseFeilmeldingFraÅr : AnnenErfaringSkjema -> AnnenErfaringSkjema
tillatÅViseFeilmeldingFraÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingFraÅr = True }


tillatÅViseFeilmeldingTilÅr : AnnenErfaringSkjema -> AnnenErfaringSkjema
tillatÅViseFeilmeldingTilÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingTilÅr = True }


tillatÅViseFeilmeldingRolle : AnnenErfaringSkjema -> AnnenErfaringSkjema
tillatÅViseFeilmeldingRolle (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingRolle = True }


tillatÅViseFeilmeldingMåned : AnnenErfaringSkjema -> AnnenErfaringSkjema
tillatÅViseFeilmeldingMåned (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingPeriode = True }


tillatÅViseAlleFeilmeldinger : AnnenErfaringSkjema -> AnnenErfaringSkjema
tillatÅViseAlleFeilmeldinger skjema =
    skjema
        |> tillatÅViseFeilmeldingFraÅr
        |> tillatÅViseFeilmeldingTilÅr
        |> tillatÅViseFeilmeldingRolle
        |> tillatÅViseFeilmeldingMåned



--- VALIDERING ---


valider : AnnenErfaringSkjema -> Maybe ValidertAnnenErfaringSkjema
valider (UvalidertSkjema info) =
    if feilmeldingRolle info.rolle /= Nothing then
        Nothing

    else if Validering.feilmeldingMaxAntallTegn info.beskrivelse 2000 /= Nothing then
        Nothing

    else
        Maybe.map
            (\periode_ ->
                ValidertSkjema
                    { rolle = info.rolle
                    , beskrivelse = info.beskrivelse
                    , periode = periode_
                    , id = info.id
                    }
            )
            (validerPeriode info.harDatoer info.fraMåned info.fraÅr info.nåværende info.tilMåned info.tilÅr)


getTilDato : Bool -> Maybe Måned -> String -> Maybe TilDato
getTilDato nåværende_ måned år =
    if nåværende_ then
        Just Nåværende

    else
        Maybe.map2 Avsluttet måned (Dato.stringTilÅr år)


validerPeriode : Bool -> Maybe Måned -> String -> Bool -> Maybe Måned -> String -> Maybe DatoPeriode
validerPeriode harDatoer_ fraMåned_ fraÅr nåværende_ tilMåned_ tilÅr =
    let
        tilDato_ =
            getTilDato nåværende_ tilMåned_ tilÅr

        maybeFraÅr =
            Dato.stringTilÅr fraÅr
    in
    if not harDatoer_ then
        -- Alt er ok hvis checkbox for dato ikke er checked
        Just IkkeOppgitt

    else
        Maybe.map3 Oppgitt fraMåned_ maybeFraÅr tilDato_


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
    case validert.periode of
        Oppgitt fraMåned_ fraÅr_ tilDato ->
            UvalidertSkjema
                { rolle = validert.rolle
                , beskrivelse = validert.beskrivelse
                , harDatoer = True
                , fraMåned = Just fraMåned_
                , fraÅr = Dato.årTilString fraÅr_
                , nåværende = tilDato == Nåværende
                , tilMåned = Just (tilDatoMåned tilDato)
                , tilÅr = tilDatoÅr tilDato
                , tillatÅViseFeilmeldingFraÅr = False
                , tillatÅViseFeilmeldingTilÅr = False
                , tillatÅViseFeilmeldingRolle = False
                , tillatÅViseFeilmeldingPeriode = False
                , id = validert.id
                }

        IkkeOppgitt ->
            UvalidertSkjema
                { rolle = validert.rolle
                , beskrivelse = validert.beskrivelse
                , harDatoer = False
                , fraMåned = Nothing
                , fraÅr = ""
                , nåværende = False
                , tilMåned = Nothing
                , tilÅr = ""
                , tillatÅViseFeilmeldingFraÅr = False
                , tillatÅViseFeilmeldingTilÅr = False
                , tillatÅViseFeilmeldingRolle = False
                , tillatÅViseFeilmeldingPeriode = False
                , id = validert.id
                }



--- ENCODE ---


encode : ValidertAnnenErfaringSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "rolle", Json.Encode.string skjema.rolle )
      , ( "beskrivelse", Json.Encode.string skjema.beskrivelse )
      ]
    , encodePeriode skjema.periode
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


encodePeriode : DatoPeriode -> List ( String, Json.Encode.Value )
encodePeriode periode_ =
    case periode_ of
        Oppgitt fraMåned_ fraÅr tilDato ->
            [ [ ( "fradato", Dato.encodeMonthYear fraMåned_ fraÅr ) ]
            , encodeTilDato tilDato
            ]
                |> List.concat

        IkkeOppgitt ->
            [ ( "naavaerende", Json.Encode.bool False ) ]
