module Api exposing
    ( hentCv
    , hentPerson
    , hentPersonalia
    , logError
    , oppdaterPersonalia
    , opprettCv
    , opprettPerson
    , opprettPersonalia
    )

import Cv.Cv as Cv exposing (Cv)
import Feilmelding exposing (Feilmelding)
import Http exposing (..)
import Personalia exposing (Personalia)
import Skjema.Personalia



--- Adressen til apiet må endres tilbake til /cv-samtale/api etter at proxy er fikset i preprod/prod


hentPerson : (Result Error () -> msg) -> Cmd msg
hentPerson msgConstructor =
    Http.get
        { url = "/cv/api/rest/person"
        , expect = expectWhatever msgConstructor
        }


opprettPerson : (Result Error () -> msg) -> Cmd msg
opprettPerson msgConstructor =
    Http.post
        { url = "/cv/api/rest/person"
        , expect = expectWhatever msgConstructor
        , body = emptyBody
        }


hentPersonalia : (Result Error Personalia -> msg) -> Cmd msg
hentPersonalia msgConstructor =
    Http.get
        { url = "/cv/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        }


opprettPersonalia : (Result Error Personalia -> msg) -> Cmd msg
opprettPersonalia msgConstructor =
    Http.post
        { url = "/cv/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = emptyBody
        }


oppdaterPersonalia : (Result Error Personalia -> msg) -> Skjema.Personalia.PersonaliaSkjema -> String -> Cmd msg
oppdaterPersonalia msgConstructor skjema id =
    put
        { url = "/cv/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = Skjema.Personalia.encode skjema id |> jsonBody
        }


hentCv : (Result Error Cv -> msg) -> Cmd msg
hentCv msgConstructor =
    Http.get
        { url = "/cv/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        }


opprettCv : (Result Error Cv -> msg) -> Cmd msg
opprettCv msgConstructor =
    Http.post
        { url = "/cv/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        , body = emptyBody
        }


logError : (Result Error () -> msg) -> Feilmelding -> Cmd msg
logError msgConstructor feilmelding =
    Http.post
        { url = "/cv-samtale/log"
        , expect = expectWhatever msgConstructor
        , body =
            feilmelding
                |> Feilmelding.encode
                |> jsonBody
        }


put :
    { url : String
    , body : Body
    , expect : Expect msg
    }
    -> Cmd msg
put r =
    request
        { method = "PUT"
        , headers = []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }
