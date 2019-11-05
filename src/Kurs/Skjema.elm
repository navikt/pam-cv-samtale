module Kurs.Skjema exposing
    ( Felt(..)
    , FullførtDato(..)
    , KursSkjema
    , ValidertKursSkjema
    , VarighetEnhet(..)
    , encode
    , feilmeldingKursholder
    , feilmeldingKursnavn
    , feilmeldingKursnavnHvisSynlig
    , feilmeldingPeriode
    , feilmeldingValgfrittFullførtÅr
    , feilmeldingVarighet
    , feilmeldingVarighetHvisSynlig
    , fullførtDatoValidert
    , fullførtMåned
    , id
    , initValidertSkjema
    , innholdTekstFelt
    , oppdaterFullførtMåned
    , oppdaterTekstFelt
    , oppdaterVarighetEnhet
    , stringTilVarighetEnhet
    , tilUvalidertSkjema
    , tillatÅViseAlleFeilmeldinger
    , tillatÅViseFeilmeldingFullførtÅr
    , tillatÅViseFeilmeldingKursnavn
    , tillatÅViseFeilmeldingVarighet
    , valider
    , validerFullført
    , varighetEnhet
    , varighetEnhetTilString
    )

import Dato exposing (Måned, År)
import Json.Encode


type KursSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type ValidertKursSkjema
    = ValidertSkjema ValidertSkjemaInfo


type FullførtDato
    = IkkeOppgitt
    | Oppgitt Måned År


type VarighetEnhet
    = Time
    | Dag
    | Uke
    | Måned


type alias UvalidertSkjemaInfo =
    { kursnavn : String
    , kursholder : String
    , fullførtMåned : Maybe Måned
    , fullførtÅr : String
    , varighet : String
    , varighetEnhet : VarighetEnhet
    , id : Maybe String
    , tillatÅViseFeilmeldingKursnavn : Bool
    , tillatÅViseFeilmeldingPeriode : Bool
    , tillatÅViseFeilmeldingFullførtÅr : Bool
    , tillatÅViseFeilmeldingVarighet : Bool
    }


type alias ValidertSkjemaInfo =
    { kursnavn : String
    , kursholder : String
    , fullførtDato : FullførtDato
    , varighet : Maybe Int
    , varighetEnhet : VarighetEnhet
    , id : Maybe String
    }



--- INIT ---


initValidertSkjema : ValidertSkjemaInfo -> ValidertKursSkjema
initValidertSkjema info =
    ValidertSkjema info



--- INNHOLD ---


type Felt
    = Kursnavn
    | Kursholder
    | FullførtÅr
    | Varighet


innholdTekstFelt : Felt -> KursSkjema -> String
innholdTekstFelt felt (UvalidertSkjema skjema) =
    case felt of
        FullførtÅr ->
            skjema.fullførtÅr

        Kursnavn ->
            skjema.kursnavn

        Kursholder ->
            skjema.kursholder

        Varighet ->
            skjema.varighet


fullførtMåned : KursSkjema -> Maybe Måned
fullførtMåned (UvalidertSkjema info) =
    info.fullførtMåned


varighetEnhet : KursSkjema -> VarighetEnhet
varighetEnhet (UvalidertSkjema info) =
    info.varighetEnhet


fullførtDatoValidert : ValidertKursSkjema -> FullførtDato
fullførtDatoValidert (ValidertSkjema validert) =
    validert.fullførtDato


id : ValidertKursSkjema -> Maybe String
id (ValidertSkjema info) =
    info.id


varighetEnhetTilString : VarighetEnhet -> String
varighetEnhetTilString enhet =
    case enhet of
        Time ->
            "Timer"

        Dag ->
            "Dager"

        Uke ->
            "Uker"

        Måned ->
            "Måneder"


stringTilVarighetEnhet : String -> VarighetEnhet
stringTilVarighetEnhet string =
    case string of
        "Timer" ->
            Time

        "Dager" ->
            Dag

        "Uker" ->
            Uke

        "Måneder" ->
            Måned

        _ ->
            Time



--- OPPDATERING ---


oppdaterTekstFelt : Felt -> String -> KursSkjema -> KursSkjema
oppdaterTekstFelt felt tekst (UvalidertSkjema skjema) =
    case felt of
        Kursnavn ->
            UvalidertSkjema { skjema | kursnavn = tekst }

        Kursholder ->
            UvalidertSkjema { skjema | kursholder = tekst }

        FullførtÅr ->
            UvalidertSkjema { skjema | fullførtÅr = tekst, tillatÅViseFeilmeldingPeriode = False }

        Varighet ->
            UvalidertSkjema { skjema | varighet = tekst }


oppdaterFullførtMåned : KursSkjema -> Maybe Måned -> KursSkjema
oppdaterFullførtMåned (UvalidertSkjema skjema) måned =
    UvalidertSkjema { skjema | fullførtMåned = måned, tillatÅViseFeilmeldingPeriode = False }


oppdaterVarighetEnhet : KursSkjema -> VarighetEnhet -> KursSkjema
oppdaterVarighetEnhet (UvalidertSkjema skjema) enhet =
    UvalidertSkjema { skjema | varighetEnhet = enhet }



--- FEILMELDINGER ---


feilmeldingKursnavn : String -> Maybe String
feilmeldingKursnavn kursnavn_ =
    if String.length (String.trim kursnavn_) == 0 then
        Just "Vennligst fyll inn et kursnavn"

    else if String.length kursnavn_ > 250 then
        let
            tallTekst =
                (String.length kursnavn_ - 250)
                    |> String.fromInt
        in
        Just ("Du har " ++ tallTekst ++ " tegn for mye")

    else
        Nothing


feilmeldingKursnavnHvisSynlig : KursSkjema -> Maybe String
feilmeldingKursnavnHvisSynlig (UvalidertSkjema skjema) =
    if skjema.tillatÅViseFeilmeldingKursnavn then
        feilmeldingKursnavn skjema.kursnavn

    else
        Nothing


feilmeldingKursholder : String -> Maybe String
feilmeldingKursholder innhold =
    if String.length innhold <= 250 then
        Nothing

    else
        let
            tallTekst =
                (String.length innhold - 250)
                    |> String.fromInt
        in
        Just ("Du har " ++ tallTekst ++ " tegn for mye")


feilmeldingPeriode : KursSkjema -> Maybe String
feilmeldingPeriode (UvalidertSkjema skjema) =
    let
        fullførtÅrErTom =
            (String.isEmpty << String.trim) skjema.fullførtÅr
    in
    if not skjema.tillatÅViseFeilmeldingPeriode then
        Nothing

    else if skjema.fullførtMåned == Nothing && fullførtÅrErTom then
        --greit å ikke fylle ut noe
        Nothing

    else if skjema.fullførtMåned /= Nothing && not fullførtÅrErTom then
        --greit å fylle ut begge
        Nothing

    else
        Just "Velger du å legge inn måned eller år må begge fylles ut."


feilmeldingVarighet : String -> Maybe String
feilmeldingVarighet varighet_ =
    if (String.isEmpty << String.trim) varighet_ then
        Nothing

    else if String.toInt varighet_ == Nothing then
        Just "Varighet kan kun inneholde tall"

    else
        Nothing


feilmeldingVarighetHvisSynlig : KursSkjema -> Maybe String
feilmeldingVarighetHvisSynlig (UvalidertSkjema skjema) =
    if skjema.tillatÅViseFeilmeldingVarighet then
        feilmeldingVarighet skjema.varighet

    else
        Nothing


feilmeldingValgfrittFullførtÅr : KursSkjema -> Maybe String
feilmeldingValgfrittFullførtÅr (UvalidertSkjema skjema) =
    if String.isEmpty skjema.fullførtÅr then
        Nothing

    else if skjema.tillatÅViseFeilmeldingFullførtÅr && (String.length skjema.fullførtÅr /= 4 || String.toInt skjema.fullførtÅr == Nothing) then
        Just "Kun 4 siffer"

    else
        Nothing


tillatÅViseFeilmeldingFullførtÅr : KursSkjema -> KursSkjema
tillatÅViseFeilmeldingFullførtÅr (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingFullførtÅr = True }


tillatÅViseFeilmeldingKursnavn : KursSkjema -> KursSkjema
tillatÅViseFeilmeldingKursnavn (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingKursnavn = True }


tillatÅViseFeilmeldingPeriode : KursSkjema -> KursSkjema
tillatÅViseFeilmeldingPeriode (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingPeriode = True }


tillatÅViseFeilmeldingVarighet : KursSkjema -> KursSkjema
tillatÅViseFeilmeldingVarighet (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | tillatÅViseFeilmeldingVarighet = True }


tillatÅViseAlleFeilmeldinger : KursSkjema -> KursSkjema
tillatÅViseAlleFeilmeldinger skjema =
    skjema
        |> tillatÅViseFeilmeldingPeriode
        |> tillatÅViseFeilmeldingFullførtÅr
        |> tillatÅViseFeilmeldingKursnavn
        |> tillatÅViseFeilmeldingVarighet



--- VALIDERING ---


valider : KursSkjema -> Maybe ValidertKursSkjema
valider (UvalidertSkjema info) =
    if feilmeldingKursnavn info.kursnavn /= Nothing then
        Nothing

    else if feilmeldingKursholder info.kursholder /= Nothing then
        Nothing

    else if feilmeldingVarighet info.varighet /= Nothing then
        Nothing

    else
        Maybe.map
            (\fullførtDato_ ->
                ValidertSkjema
                    { kursnavn = info.kursnavn
                    , kursholder = info.kursholder
                    , fullførtDato = fullførtDato_
                    , varighet = String.toInt info.varighet
                    , varighetEnhet = info.varighetEnhet
                    , id = info.id
                    }
            )
            (validerFullført info.fullførtMåned info.fullførtÅr)


validerFullført : Maybe Måned -> String -> Maybe FullførtDato
validerFullført fullførtMåned_ fullførtÅr_ =
    let
        maybeFullførtÅr =
            Dato.stringTilÅr fullførtÅr_
    in
    if fullførtMåned_ == Nothing && String.isEmpty fullførtÅr_ then
        -- Alt er ok hvis ingen datoer valgt
        Just IkkeOppgitt

    else
        Maybe.map2 Oppgitt fullførtMåned_ maybeFullførtÅr


tilUvalidertSkjema : ValidertKursSkjema -> KursSkjema
tilUvalidertSkjema (ValidertSkjema validert) =
    UvalidertSkjema
        { kursnavn = validert.kursnavn
        , kursholder = validert.kursholder
        , fullførtMåned =
            case validert.fullførtDato of
                IkkeOppgitt ->
                    Nothing

                Oppgitt måned _ ->
                    Just måned
        , fullførtÅr =
            case validert.fullførtDato of
                IkkeOppgitt ->
                    ""

                Oppgitt _ år ->
                    Dato.årTilString år
        , varighet =
            case validert.varighet of
                Nothing ->
                    ""

                Just varighet_ ->
                    String.fromInt varighet_
        , varighetEnhet = validert.varighetEnhet
        , tillatÅViseFeilmeldingKursnavn = False
        , tillatÅViseFeilmeldingPeriode = False
        , tillatÅViseFeilmeldingFullførtÅr = False
        , tillatÅViseFeilmeldingVarighet = False
        , id = validert.id
        }



--- ENCODE ---


encode : ValidertKursSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "tittel", Json.Encode.string skjema.kursnavn )
      , ( "tidspunkt", encodeFullført skjema.fullførtDato )
      , ( "utsteder", Json.Encode.string skjema.kursholder )
      , ( "varighet", encodeVarighet skjema.varighet )
      , ( "varighetEnhet", encodeVarighetEnhet skjema.varighetEnhet )
      ]
    ]
        |> List.concat
        |> Json.Encode.object


encodeFullført : FullførtDato -> Json.Encode.Value
encodeFullført dato =
    case dato of
        IkkeOppgitt ->
            Json.Encode.null

        Oppgitt måned år ->
            Dato.encodeMonthYear måned år


encodeVarighet : Maybe Int -> Json.Encode.Value
encodeVarighet varighet_ =
    case varighet_ of
        Nothing ->
            Json.Encode.null

        Just value ->
            Json.Encode.int value


encodeVarighetEnhet : VarighetEnhet -> Json.Encode.Value
encodeVarighetEnhet enhet =
    (case enhet of
        Time ->
            "TIME"

        Dag ->
            "DAG"

        Uke ->
            "UKE"

        Måned ->
            "MND"
    )
        |> Json.Encode.string
