module SporsmalViewState exposing
    ( IkonStatus(..)
    , SpørsmålStyle(..)
    , SpørsmålViewState
    , id
    , ikonStatus
    , init
    , initFerdigAnimert
    , initFerdigAnimertFørNyMelding
    , initFerdigKalkulert
    , initKalkuleres
    , initSkriver
    , medIkonForFørsteMelding
    , meldingsType
    , spørsmålStyle
    , tekst
    , utenIkon
    )

import Melding exposing (Melding, MeldingsType(..), Tekstområde(..))


type SpørsmålViewState
    = SpørsmålViewState
        { melding : Melding
        , style : SpørsmålStyle
        , ikonStatus : IkonStatus
        , id : String
        }


type SpørsmålStyle
    = FørSkriveindikator
    | Skriveindikator
    | StørrelseKalkuleres
    | MeldingAnimeres { height : Int, width : Int }
    | MeldingFerdigAnimert


type IkonStatus
    = SkjultIkon
    | MidtstiltIkonForFørsteSpørsmål
    | MidtstiltIkon
    | IkonForNesteMelding { height : Int }



--- INIT ---


init : Melding -> String -> SpørsmålViewState
init melding id_ =
    SpørsmålViewState
        { melding = melding
        , style = FørSkriveindikator
        , ikonStatus = SkjultIkon
        , id = id_
        }


initSkriver : Melding -> String -> SpørsmålViewState
initSkriver melding id_ =
    SpørsmålViewState
        { melding = melding
        , style = Skriveindikator
        , ikonStatus = SkjultIkon
        , id = id_
        }


medIkonForFørsteMelding : SpørsmålViewState -> SpørsmålViewState
medIkonForFørsteMelding (SpørsmålViewState info) =
    SpørsmålViewState { info | ikonStatus = MidtstiltIkonForFørsteSpørsmål }


initKalkuleres : Melding -> String -> SpørsmålViewState
initKalkuleres melding id_ =
    SpørsmålViewState
        { melding = melding
        , style = StørrelseKalkuleres
        , ikonStatus = MidtstiltIkon
        , id = id_
        }


initFerdigKalkulert : { height : Int, width : Int } -> Melding -> String -> SpørsmålViewState
initFerdigKalkulert { height, width } melding id_ =
    SpørsmålViewState
        { melding = melding
        , style = MeldingAnimeres { height = height, width = width }
        , ikonStatus = MidtstiltIkon
        , id = id_
        }


initFerdigAnimert : Melding -> String -> SpørsmålViewState
initFerdigAnimert melding id_ =
    SpørsmålViewState
        { melding = melding
        , style = MeldingFerdigAnimert
        , ikonStatus = MidtstiltIkon
        , id = id_
        }


utenIkon : SpørsmålViewState -> SpørsmålViewState
utenIkon (SpørsmålViewState info) =
    SpørsmålViewState { info | ikonStatus = SkjultIkon }


initFerdigAnimertFørNyMelding : { height : Int } -> Melding -> String -> SpørsmålViewState
initFerdigAnimertFørNyMelding { height } melding id_ =
    SpørsmålViewState
        { melding = melding
        , style = MeldingFerdigAnimert
        , ikonStatus = IkonForNesteMelding { height = height }
        , id = id_
        }



--- INNHOLD ---


tekst : SpørsmålViewState -> List Tekstområde
tekst (SpørsmålViewState info) =
    Melding.innhold info.melding


spørsmålStyle : SpørsmålViewState -> SpørsmålStyle
spørsmålStyle (SpørsmålViewState info) =
    info.style


ikonStatus : SpørsmålViewState -> IkonStatus
ikonStatus (SpørsmålViewState info) =
    info.ikonStatus


id : SpørsmålViewState -> String
id (SpørsmålViewState info) =
    info.id


meldingsType : SpørsmålViewState -> MeldingsType
meldingsType (SpørsmålViewState info) =
    Melding.meldingstype info.melding


