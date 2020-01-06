module Forerkort.ForerkortKode exposing
    ( FørerkortKode
    , encode
    , klasseB
    , kode
    , liste
    , spørOmDatoInfo
    , stringTilMaybeFørerkortKode
    , term
    )

import Json.Encode
import List.Extra as List


type FørerkortKode
    = FørerkortKode FørerkortKodeInfo


type alias FørerkortKodeInfo =
    { kode : String
    , term : String
    , spørOmDatoInfo : Bool
    }


spørOmDatoInfo : FørerkortKode -> Bool
spørOmDatoInfo (FørerkortKode info) =
    info.spørOmDatoInfo


kode : FørerkortKode -> String
kode (FørerkortKode info) =
    info.kode


term : FørerkortKode -> String
term (FørerkortKode info) =
    info.term


klasseB : FørerkortKode
klasseB =
    FørerkortKode
        { kode = "B"
        , term = "Personbil"
        , spørOmDatoInfo = False
        }


stringTilMaybeFørerkortKode : String -> Maybe FørerkortKode
stringTilMaybeFørerkortKode valgtFørerkort =
    case List.find (\forerkortKode -> valgtFørerkort == kode forerkortKode) liste of
        Just førerkortKode ->
            Just førerkortKode

        Nothing ->
            Nothing


liste : List FørerkortKode
liste =
    [ klasseB
    , FørerkortKode
        { kode = "BE"
        , term = "Personbil med tilhenger"
        , spørOmDatoInfo = False
        }
    , FørerkortKode
        { kode = "C"
        , term = "Lastebil"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "C1"
        , term = "Lett lastebil"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "C1E"
        , term = "Lett lastebil med tilhenger"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "CE"
        , term = "Lastebil med tilhenger"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "D1"
        , term = "Minibuss"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "D1E"
        , term = "Minibuss med tilhenger"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "D"
        , term = "Buss"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "DE"
        , term = "Buss med tilhenger"
        , spørOmDatoInfo = True
        }
    , FørerkortKode
        { kode = "AM"
        , term = "Moped"
        , spørOmDatoInfo = False
        }
    , FørerkortKode
        { kode = "A1"
        , term = "Lett motorsykkel"
        , spørOmDatoInfo = False
        }
    , FørerkortKode
        { kode = "A2"
        , term = "Mellomtung motorsykkel"
        , spørOmDatoInfo = False
        }
    , FørerkortKode
        { kode = "A"
        , term = "Tung motorsykkel"
        , spørOmDatoInfo = False
        }
    , FørerkortKode
        { kode = "T"
        , term = "Traktor"
        , spørOmDatoInfo = False
        }
    , FørerkortKode
        { kode = "S"
        , term = "Snøscooter"
        , spørOmDatoInfo = False
        }
    ]



--- ENCODER ---


encode : FørerkortKode -> Json.Encode.Value
encode (FørerkortKode info) =
    Json.Encode.string info.kode
