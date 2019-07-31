module Skjema.Sprak exposing (SpråkSkjema(..), encode, init)

import Cv.Spraakferdighet as Spraakferdighet exposing (Spraakferdighet)
import Json.Encode


type SpråkSkjema
    = SpråkSkjema SpråkSkjemaInfo


type alias SpråkSkjemaInfo =
    { sprak : String
    , muntlig : String
    , skriftlig : String
    }


språk : SpråkSkjema -> String
språk (SpråkSkjema info) =
    info.sprak


muntlig : SpråkSkjema -> String
muntlig (SpråkSkjema info) =
    info.muntlig


skriftlig : SpråkSkjema -> String
skriftlig (SpråkSkjema info) =
    info.skriftlig


init : String -> String -> String -> SpråkSkjema
init skjemaSpråk skjemaMuntlig skjemaSkriftlig =
    SpråkSkjema
        { sprak = skjemaSpråk
        , muntlig = skjemaMuntlig
        , skriftlig = skjemaSkriftlig
        }


encode : SpråkSkjema -> Json.Encode.Value
encode (SpråkSkjema info) =
    Json.Encode.object
        [ ( "sprak", Json.Encode.string info.sprak )
        , ( "muntlig", Json.Encode.string info.muntlig )
        , ( "skriftlig", Json.Encode.string info.skriftlig )
        ]
