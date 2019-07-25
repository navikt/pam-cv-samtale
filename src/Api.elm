module Api exposing
    ( hentAAreg
    , hentCv
    , hentPerson
    , hentPersonalia
    , hentYrkeTypeahead
    , lagreArbeidserfaring
    , logError
    , oppdaterPersonalia
    , opprettCv
    , opprettPerson
    , opprettPersonalia
    )

import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Cv.Cv as Cv exposing (Cv)
import Feilmelding exposing (Feilmelding)
import Http exposing (..)
import Json.Decode
import Personalia exposing (Personalia)
import Skjema.ArbeidserfaringSkjema
import Skjema.Personalia
import YrkeTypeahead as YrkeTypahead exposing (YrkeTypeahead)


hentPerson : (Result Error () -> msg) -> Cmd msg
hentPerson msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person"
        , expect = expectWhatever msgConstructor
        }


opprettPerson : (Result Error () -> msg) -> Cmd msg
opprettPerson msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person"
        , expect = expectWhatever msgConstructor
        , body = emptyBody
        }


hentPersonalia : (Result Error Personalia -> msg) -> Cmd msg
hentPersonalia msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        }


opprettPersonalia : (Result Error Personalia -> msg) -> Cmd msg
opprettPersonalia msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = emptyBody
        }


oppdaterPersonalia : (Result Error Personalia -> msg) -> Skjema.Personalia.PersonaliaSkjema -> String -> Cmd msg
oppdaterPersonalia msgConstructor skjema id =
    put
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = Skjema.Personalia.encode skjema id |> jsonBody
        }


hentCv : (Result Error Cv -> msg) -> Cmd msg
hentCv msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        }


opprettCv : (Result Error Cv -> msg) -> Cmd msg
opprettCv msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        , body = emptyBody
        }


hentAAreg : (Result Error (List Arbeidserfaring) -> msg) -> Cmd msg
hentAAreg msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv/aareg"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        }


lagreArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Skjema.ArbeidserfaringSkjema.ArbeidserfaringSkjema -> Cmd msg
lagreArbeidserfaring msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/v2/arbeidserfaring"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        , body = Skjema.ArbeidserfaringSkjema.encode skjema |> jsonBody
        }


hentYrkeTypeahead : (Result Error (List YrkeTypeahead) -> msg) -> String -> Cmd msg
hentYrkeTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/yrke?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list YrkeTypahead.decode)
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
