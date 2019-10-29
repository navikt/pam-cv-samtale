module Forerkort.Skjema exposing
    ( FørerkortSkjema
    , FørerkortSkjemaInfo
    , encode
    , førerkortNavn
    , init
    , klasseB
    )

import FørerkortKode exposing (FørerkortKode)
import Json.Encode


type FørerkortSkjema
    = FørerkortSkjema FørerkortSkjemaInfo


type alias FørerkortSkjemaInfo =
    { førerkort : FørerkortKode
    }


klasseB =
    FørerkortSkjema
        { førerkort = FørerkortKode.klasseB
        }


førerkortNavn : FørerkortSkjema -> String
førerkortNavn (FørerkortSkjema info) =
    FørerkortKode.term info.førerkort


init : FørerkortSkjemaInfo -> FørerkortSkjema
init info =
    FørerkortSkjema info


encode : FørerkortSkjema -> Json.Encode.Value
encode (FørerkortSkjema info) =
    Json.Encode.object
        [ ( "klasse", FørerkortKode.encode info.førerkort )
        ]
