module FørerkortKode exposing
    ( FørerkortKode
    , encode
    , klasseB
    , kode
    , liste
    , term
    )

import Json.Encode


type FørerkortKode
    = FørerkortKode FørerkortKodeInfo


type alias FørerkortKodeInfo =
    { kode : String
    , term : String
    }


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
        }


liste : List FørerkortKode
liste =
    [ klasseB
    , FørerkortKode
        { kode = "BE"
        , term = "Personbil med tilhenger"
        }
    , FørerkortKode
        { kode = "C"
        , term = "Lastebil"
        }
    , FørerkortKode
        { kode = "C1"
        , term = "Lett lastebil"
        }
    , FørerkortKode
        { kode = "C1E"
        , term = "Lett lastebil med tilhenger"
        }
    , FørerkortKode
        { kode = "CE"
        , term = "Lastebil med tilhenger"
        }
    , FørerkortKode
        { kode = "D1"
        , term = "Minibuss"
        }
    , FørerkortKode
        { kode = "D1E"
        , term = "Minibuss med tilhenger"
        }
    , FørerkortKode
        { kode = "AM"
        , term = "Moped"
        }
    , FørerkortKode
        { kode = "A1"
        , term = "Lett motorsykkel"
        }
    , FørerkortKode
        { kode = "A2"
        , term = "Mellomtung motorsykkel"
        }
    , FørerkortKode
        { kode = "A"
        , term = "Tung motorsykkel"
        }
    , FørerkortKode
        { kode = "T"
        , term = "Traktor"
        }
    , FørerkortKode
        { kode = "S"
        , term = "Snøscooter"
        }
    ]



--- ENCODER ---


encode : FørerkortKode -> Json.Encode.Value
encode (FørerkortKode info) =
    Json.Encode.string info.kode
