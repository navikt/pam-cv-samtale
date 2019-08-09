module MeldingsLogg exposing
    ( FerdigAnimertMeldingsLogg
    , FerdigAnimertStatus(..)
    , MeldingsLogg
    , SkriveStatus(..)
    , ferdigAnimert
    , fullførMelding
    , init
    , leggTilSpørsmål
    , leggTilSvar
    , meldinger
    , nesteMeldingToString
    , skriveStatus
    , startÅSkrive
    , tilMeldingsLogg
    )

import Melding exposing (Melding)


type MeldingsLogg
    = MeldingsLogg MeldigsLoggInfo


type FerdigAnimertMeldingsLogg
    = FerdigAnimertMeldingsLogg (List Melding)


type alias MeldigsLoggInfo =
    { meldingsLogg : List Melding
    , ikkeVist : UskrevneMeldinger
    }


type UskrevneMeldinger
    = Ingen
    | SkriverEnMelding Melding (List Melding)
    | LeserMelding Melding (List Melding)


type SkriveStatus
    = Skriver
    | SkriverIkke


init : MeldingsLogg
init =
    MeldingsLogg
        { meldingsLogg = []
        , ikkeVist = Ingen
        }


meldinger : MeldingsLogg -> List Melding
meldinger (MeldingsLogg { meldingsLogg }) =
    meldingsLogg


nesteMeldingToString : MeldingsLogg -> Float
nesteMeldingToString (MeldingsLogg { ikkeVist }) =
    case ikkeVist of
        Ingen ->
            1.0

        SkriverEnMelding melding list ->
            let
                venteTid =
                    String.toFloat (String.fromInt (List.length (Melding.innhold melding))) |> Maybe.withDefault 1.0
            in
            if venteTid > 3.0 then
                3.0

            else
                venteTid

        LeserMelding melding list ->
            let
                venteTid =
                    String.toFloat (String.fromInt (List.length (Melding.innhold melding))) |> Maybe.withDefault 1.0
            in
            if venteTid > 3.0 then
                3.0

            else
                venteTid


skriveStatus : MeldingsLogg -> SkriveStatus
skriveStatus (MeldingsLogg { ikkeVist }) =
    case ikkeVist of
        Ingen ->
            SkriverIkke

        SkriverEnMelding _ _ ->
            Skriver

        LeserMelding _ _ ->
            SkriverIkke


leggTilSpørsmål : List Melding -> MeldingsLogg -> MeldingsLogg
leggTilSpørsmål nyeMeldinger (MeldingsLogg info) =
    MeldingsLogg
        { info
            | ikkeVist =
                case nyeMeldinger of
                    first :: rest ->
                        case info.ikkeVist of
                            Ingen ->
                                LeserMelding first rest

                            SkriverEnMelding melding list ->
                                SkriverEnMelding melding (list ++ nyeMeldinger)

                            LeserMelding melding list ->
                                LeserMelding melding (list ++ nyeMeldinger)

                    [] ->
                        Ingen
        }


leggTilSvar : Melding -> MeldingsLogg -> MeldingsLogg
leggTilSvar nyMelding (MeldingsLogg info) =
    MeldingsLogg { info | meldingsLogg = info.meldingsLogg ++ [ nyMelding ] }


startÅSkrive : MeldingsLogg -> MeldingsLogg
startÅSkrive (MeldingsLogg info) =
    MeldingsLogg
        { info
            | ikkeVist =
                case info.ikkeVist of
                    Ingen ->
                        Ingen

                    SkriverEnMelding melding list ->
                        SkriverEnMelding melding list

                    LeserMelding melding list ->
                        SkriverEnMelding melding list
        }


fullførMelding : MeldingsLogg -> MeldingsLogg
fullførMelding (MeldingsLogg info) =
    MeldingsLogg
        { info
            | meldingsLogg =
                case info.ikkeVist of
                    Ingen ->
                        info.meldingsLogg

                    SkriverEnMelding melding _ ->
                        info.meldingsLogg ++ [ melding ]

                    LeserMelding _ _ ->
                        info.meldingsLogg
            , ikkeVist =
                case info.ikkeVist of
                    Ingen ->
                        Ingen

                    SkriverEnMelding _ list ->
                        case list of
                            first :: rest ->
                                LeserMelding first rest

                            [] ->
                                Ingen

                    LeserMelding melding list ->
                        LeserMelding melding list
        }


type FerdigAnimertStatus
    = FerdigAnimert FerdigAnimertMeldingsLogg
    | MeldingerGjenstår


ferdigAnimert : MeldingsLogg -> FerdigAnimertStatus
ferdigAnimert (MeldingsLogg info) =
    case info.ikkeVist of
        Ingen ->
            FerdigAnimert (FerdigAnimertMeldingsLogg info.meldingsLogg)

        SkriverEnMelding melding list ->
            MeldingerGjenstår

        LeserMelding melding list ->
            MeldingerGjenstår


tilMeldingsLogg : FerdigAnimertMeldingsLogg -> MeldingsLogg
tilMeldingsLogg (FerdigAnimertMeldingsLogg meldingslogg) =
    MeldingsLogg
        { meldingsLogg = meldingslogg
        , ikkeVist = Ingen
        }
