module Forerkort.Skjema exposing
    ( FørerkortSkjema
    , FørerkortSkjemaInfo
    , ValidertFørerkortSkjema
    , encode
    , fraDagFraSkjema
    , fraDatoFraValidertSkjema
    , fraFørerkortKode
    , fraMånedFraSkjema
    , fraÅrFraSkjema
    , førerkortFraValidertSkjema
    , førerkortKodeFraSkjema
    , init
    , initValidert
    , klasseB
    , oppdaterFraDag
    , oppdaterFraMåned
    , oppdaterFraÅr
    , oppdaterFørerkort
    , oppdaterTilDag
    , oppdaterTilMåned
    , oppdaterTilÅr
    , tilDagFraSkjema
    , tilDatoFraValidertSkjema
    , tilMånedFraSkjema
    , tilÅrFraSkjema
    , uvalidertSkjemaFraValidertSkjema
    , valider
    )

import Dato exposing (Dato, Måned)
import FørerkortKode exposing (FørerkortKode)
import Json.Encode


type FørerkortSkjema
    = FørerkortSkjema FørerkortSkjemaInfo


type alias ValidertFørerkortSkjemaInfo =
    { førerkort : FørerkortKode
    , fraDato : Maybe Dato
    , tilDato : Maybe Dato
    }


type ValidertFørerkortSkjema
    = ValidertFørerkortSkjema ValidertFørerkortSkjemaInfo


type alias FørerkortSkjemaInfo =
    { førerkort : Maybe FørerkortKode
    , fraÅr : String
    , fraMåned : Maybe Måned
    , fraDag : String
    , tilÅr : String
    , tilMåned : Maybe Måned
    , tilDag : String
    }


oppdaterFørerkort : FørerkortSkjema -> Maybe FørerkortKode -> FørerkortSkjema
oppdaterFørerkort (FørerkortSkjema info) kode =
    FørerkortSkjema { info | førerkort = kode }


oppdaterFraÅr : FørerkortSkjema -> String -> FørerkortSkjema
oppdaterFraÅr (FørerkortSkjema info) fraÅr =
    FørerkortSkjema { info | fraÅr = fraÅr }


oppdaterFraMåned : FørerkortSkjema -> Maybe Måned -> FørerkortSkjema
oppdaterFraMåned (FørerkortSkjema info) fraMåned =
    FørerkortSkjema { info | fraMåned = fraMåned }


oppdaterFraDag : FørerkortSkjema -> String -> FørerkortSkjema
oppdaterFraDag (FørerkortSkjema info) fraDag =
    FørerkortSkjema { info | fraDag = fraDag }


oppdaterTilÅr : FørerkortSkjema -> String -> FørerkortSkjema
oppdaterTilÅr (FørerkortSkjema info) tilÅr =
    FørerkortSkjema { info | tilÅr = tilÅr }


oppdaterTilMåned : FørerkortSkjema -> Maybe Måned -> FørerkortSkjema
oppdaterTilMåned (FørerkortSkjema info) tilMåned =
    FørerkortSkjema { info | tilMåned = tilMåned }


oppdaterTilDag : FørerkortSkjema -> String -> FørerkortSkjema
oppdaterTilDag (FørerkortSkjema info) tilDag =
    FørerkortSkjema { info | tilDag = tilDag }


førerkortKodeFraSkjema : FørerkortSkjema -> Maybe FørerkortKode
førerkortKodeFraSkjema (FørerkortSkjema info) =
    info.førerkort


uvalidertSkjemaFraValidertSkjema : ValidertFørerkortSkjema -> FørerkortSkjema
uvalidertSkjemaFraValidertSkjema (ValidertFørerkortSkjema info) =
    FørerkortSkjema
        { førerkort = Just info.førerkort
        , fraÅr =
            info.fraDato
                |> Maybe.map Dato.getDatoÅr
                |> Maybe.withDefault ""
        , fraMåned =
            info.fraDato
                |> Maybe.map Dato.getDatoMåned
        , fraDag =
            info.fraDato
                |> Maybe.map Dato.getDatoDag
                |> Maybe.withDefault ""
        , tilÅr =
            info.tilDato
                |> Maybe.map Dato.getDatoÅr
                |> Maybe.withDefault ""
        , tilMåned =
            info.tilDato
                |> Maybe.map Dato.getDatoMåned
        , tilDag =
            info.tilDato
                |> Maybe.map Dato.getDatoDag
                |> Maybe.withDefault ""
        }


førerkortFraValidertSkjema : ValidertFørerkortSkjema -> String
førerkortFraValidertSkjema (ValidertFørerkortSkjema info) =
    FørerkortKode.term info.førerkort


fraDatoFraValidertSkjema : ValidertFørerkortSkjema -> String
fraDatoFraValidertSkjema (ValidertFørerkortSkjema info) =
    case info.fraDato of
        Just dato ->
            Dato.toString dato

        Nothing ->
            ""


fraDagFraSkjema : FørerkortSkjema -> String
fraDagFraSkjema (FørerkortSkjema info) =
    info.fraDag


fraÅrFraSkjema : FørerkortSkjema -> String
fraÅrFraSkjema (FørerkortSkjema info) =
    info.fraÅr


fraMånedFraSkjema : FørerkortSkjema -> Måned
fraMånedFraSkjema (FørerkortSkjema info) =
    case info.fraMåned of
        Just måned ->
            måned

        Nothing ->
            Dato.Ikke_valgt


tilDagFraSkjema : FørerkortSkjema -> String
tilDagFraSkjema (FørerkortSkjema info) =
    info.tilDag


tilÅrFraSkjema : FørerkortSkjema -> String
tilÅrFraSkjema (FørerkortSkjema info) =
    info.tilÅr


tilMånedFraSkjema : FørerkortSkjema -> Måned
tilMånedFraSkjema (FørerkortSkjema info) =
    case info.tilMåned of
        Just måned ->
            måned

        Nothing ->
            Dato.Ikke_valgt


tilDatoFraValidertSkjema : ValidertFørerkortSkjema -> String
tilDatoFraValidertSkjema (ValidertFørerkortSkjema info) =
    case info.tilDato of
        Just dato ->
            Dato.toString dato

        Nothing ->
            ""


valider : FørerkortSkjema -> Maybe ValidertFørerkortSkjema
valider (FørerkortSkjema uvalidert) =
    case uvalidert.førerkort of
        Just førerkort ->
            case Dato.validerDato { dag = uvalidert.fraDag, måned = uvalidert.fraMåned, år = uvalidert.fraÅr } of
                Dato.DatoValiderer fraDato ->
                    case Dato.validerDato { dag = uvalidert.tilDag, måned = uvalidert.tilMåned, år = uvalidert.tilÅr } of
                        Dato.DatoValiderer tilDato ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Just fraDato, tilDato = Just tilDato })

                        Dato.DatoValidererIkke ->
                            Nothing

                        Dato.DatoIkkeSkrevetInn ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Just fraDato, tilDato = Nothing })

                Dato.DatoValidererIkke ->
                    Nothing

                Dato.DatoIkkeSkrevetInn ->
                    case Dato.validerDato { dag = uvalidert.tilDag, måned = uvalidert.tilMåned, år = uvalidert.tilÅr } of
                        Dato.DatoValiderer tilDato ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Nothing, tilDato = Just tilDato })

                        Dato.DatoValidererIkke ->
                            Nothing

                        Dato.DatoIkkeSkrevetInn ->
                            Just (ValidertFørerkortSkjema { førerkort = førerkort, fraDato = Nothing, tilDato = Nothing })

        Nothing ->
            Nothing


initValidert : ValidertFørerkortSkjemaInfo -> ValidertFørerkortSkjema
initValidert validertFørerkortSkjemaInfo =
    ValidertFørerkortSkjema validertFørerkortSkjemaInfo


fraFørerkortKode : FørerkortKode -> ValidertFørerkortSkjema
fraFørerkortKode kode =
    ValidertFørerkortSkjema { førerkort = kode, fraDato = Nothing, tilDato = Nothing }


klasseB : ValidertFørerkortSkjema
klasseB =
    ValidertFørerkortSkjema { førerkort = FørerkortKode.klasseB, fraDato = Nothing, tilDato = Nothing }


init : FørerkortSkjemaInfo -> FørerkortSkjema
init info =
    FørerkortSkjema info


encode : ValidertFørerkortSkjema -> Json.Encode.Value
encode (ValidertFørerkortSkjema info) =
    Json.Encode.object
        [ ( "klasse", FørerkortKode.encode info.førerkort )
        , ( "fraDato", Dato.encodeMaybeDato info.fraDato )
        , ( "utloperDato", Dato.encodeMaybeDato info.tilDato )
        ]
