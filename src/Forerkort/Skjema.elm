module Forerkort.Skjema exposing
    ( FørerkortSkjema
    , FørerkortSkjemaInfo
    , ValidertFørerkortSkjema
    , encode
    , feilmeldingFraDato
    , feilmeldingTilDato
    , fraDatoFraSkjema
    , fraDatoFraValidertSkjema
    , fraFørerkortKode
    , førerkortFraValidertSkjema
    , førerkortKodeFraSkjema
    , init
    , initValidert
    , klasseB
    , oppdaterFraDato
    , oppdaterFørerkort
    , oppdaterTilDato
    , tilDatoFraSkjema
    , tilDatoFraValidertSkjema
    , tillatÅViseAlleFeilmeldinger
    , tillatÅViseFeilmeldingFraDato
    , tillatÅViseFeilmeldingTilDato
    , uvalidertSkjemaFraValidertSkjema
    , valider
    )

import Dato.Dato as Dato
import Forerkort.ForerkortKode as FørerkortKode exposing (FørerkortKode)
import Json.Encode


type FørerkortSkjema
    = FørerkortSkjema FørerkortSkjemaInfo


type alias ValidertFørerkortSkjemaInfo =
    { førerkort : FørerkortKode
    , fraDato : Maybe String
    , tilDato : Maybe String
    }


type ValidertFørerkortSkjema
    = ValidertFørerkortSkjema ValidertFørerkortSkjemaInfo


type alias FørerkortSkjemaInfo =
    { førerkort : Maybe FørerkortKode
    , fraDato : String
    , tilDato : String
    , tillatÅViseFeilmeldingFraDato : Bool
    , tillatÅViseFeilmeldingTilDato : Bool
    }



--- INIT ---


init : FørerkortSkjemaInfo -> FørerkortSkjema
init info =
    FørerkortSkjema info


initValidert : ValidertFørerkortSkjemaInfo -> ValidertFørerkortSkjema
initValidert validertFørerkortSkjemaInfo =
    ValidertFørerkortSkjema validertFørerkortSkjemaInfo



--- INNHOLD ---


klasseB : ValidertFørerkortSkjema
klasseB =
    ValidertFørerkortSkjema { førerkort = FørerkortKode.klasseB, fraDato = Nothing, tilDato = Nothing }


førerkortKodeFraSkjema : FørerkortSkjema -> Maybe FørerkortKode
førerkortKodeFraSkjema (FørerkortSkjema info) =
    info.førerkort


fraDatoFraValidertSkjema : ValidertFørerkortSkjema -> String
fraDatoFraValidertSkjema (ValidertFørerkortSkjema info) =
    case info.fraDato of
        Just dato ->
            dato

        Nothing ->
            ""


førerkortFraValidertSkjema : ValidertFørerkortSkjema -> String
førerkortFraValidertSkjema (ValidertFørerkortSkjema info) =
    FørerkortKode.term info.førerkort


tilDatoFraValidertSkjema : ValidertFørerkortSkjema -> String
tilDatoFraValidertSkjema (ValidertFørerkortSkjema info) =
    case info.tilDato of
        Just dato ->
            dato

        Nothing ->
            ""


fraDatoFraSkjema : FørerkortSkjema -> String
fraDatoFraSkjema (FørerkortSkjema info) =
    info.fraDato


tilDatoFraSkjema : FørerkortSkjema -> String
tilDatoFraSkjema (FørerkortSkjema info) =
    info.tilDato


fraFørerkortKode : FørerkortKode -> ValidertFørerkortSkjema
fraFørerkortKode kode =
    ValidertFørerkortSkjema { førerkort = kode, fraDato = Nothing, tilDato = Nothing }



--- OPPDATERING ---


oppdaterFørerkort : FørerkortSkjema -> Maybe FørerkortKode -> FørerkortSkjema
oppdaterFørerkort (FørerkortSkjema info) kode =
    FørerkortSkjema { info | førerkort = kode }


oppdaterFraDato : FørerkortSkjema -> String -> FørerkortSkjema
oppdaterFraDato (FørerkortSkjema info) fraDag =
    FørerkortSkjema { info | fraDato = fraDag }


oppdaterTilDato : FørerkortSkjema -> String -> FørerkortSkjema
oppdaterTilDato (FørerkortSkjema info) tilDag =
    FørerkortSkjema { info | tilDato = tilDag }



--- VALIDERING ---


valider : FørerkortSkjema -> Maybe ValidertFørerkortSkjema
valider (FørerkortSkjema uvalidert) =
    case uvalidert.førerkort of
        Just førerkort ->
            case Dato.validerDato uvalidert.fraDato of
                Dato.GyldigDato fraDato ->
                    case Dato.validerDato uvalidert.tilDato of
                        Dato.GyldigDato tilDato ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Just fraDato, tilDato = Just tilDato })

                        Dato.DatoValideringsfeil _ ->
                            Nothing

                        Dato.DatoIkkeSkrevetInn ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Just fraDato, tilDato = Nothing })

                Dato.DatoValideringsfeil _ ->
                    Nothing

                Dato.DatoIkkeSkrevetInn ->
                    case Dato.validerDato uvalidert.tilDato of
                        Dato.GyldigDato tilDato ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Nothing, tilDato = Just tilDato })

                        Dato.DatoValideringsfeil _ ->
                            Nothing

                        Dato.DatoIkkeSkrevetInn ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Nothing, tilDato = Nothing })

        Nothing ->
            Nothing


uvalidertSkjemaFraValidertSkjema : ValidertFørerkortSkjema -> FørerkortSkjema
uvalidertSkjemaFraValidertSkjema (ValidertFørerkortSkjema info) =
    FørerkortSkjema
        { førerkort = Just info.førerkort
        , fraDato =
            info.fraDato
                |> Maybe.withDefault ""
        , tilDato =
            info.tilDato
                |> Maybe.withDefault ""
        , tillatÅViseFeilmeldingFraDato = False
        , tillatÅViseFeilmeldingTilDato = False
        }


feilmeldingFraDato : FørerkortSkjema -> Maybe String
feilmeldingFraDato (FørerkortSkjema skjema) =
    if skjema.tillatÅViseFeilmeldingFraDato then
        Dato.feilmeldingForDato skjema.fraDato

    else
        Nothing


feilmeldingTilDato : FørerkortSkjema -> Maybe String
feilmeldingTilDato (FørerkortSkjema skjema) =
    if skjema.tillatÅViseFeilmeldingTilDato then
        Dato.feilmeldingForDato skjema.tilDato

    else
        Nothing


tillatÅViseFeilmeldingFraDato : FørerkortSkjema -> FørerkortSkjema
tillatÅViseFeilmeldingFraDato (FørerkortSkjema skjema) =
    FørerkortSkjema { skjema | tillatÅViseFeilmeldingFraDato = True }


tillatÅViseFeilmeldingTilDato : FørerkortSkjema -> FørerkortSkjema
tillatÅViseFeilmeldingTilDato (FørerkortSkjema skjema) =
    FørerkortSkjema { skjema | tillatÅViseFeilmeldingTilDato = True }


tillatÅViseAlleFeilmeldinger : FørerkortSkjema -> FørerkortSkjema
tillatÅViseAlleFeilmeldinger skjema =
    skjema
        |> tillatÅViseFeilmeldingFraDato
        |> tillatÅViseFeilmeldingTilDato



--- ENCODE ---


encode : ValidertFørerkortSkjema -> Json.Encode.Value
encode (ValidertFørerkortSkjema info) =
    Json.Encode.object
        [ ( "klasse", FørerkortKode.encode info.førerkort )
        , ( "fraDato"
          , info.fraDato
                |> Maybe.map Dato.encodeDato
                |> Maybe.withDefault Json.Encode.null
          )
        , ( "utloperDato"
          , info.tilDato
                |> Maybe.map Dato.encodeDato
                |> Maybe.withDefault Json.Encode.null
          )
        ]
