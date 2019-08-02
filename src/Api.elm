module Api exposing
    ( getAAreg
    , getAutorisasjonTypeahead
    , getCv
    , getFagbrevTypeahead
    , getMesterbrevTypeahead
    , getPerson
    , getPersonalia
    , getSpråkkoder
    , getYrkeTypeahead
    , logError
    , postArbeidserfaring
    , postCv
    , postFagdokumentasjon
    , postPerson
    , postPersonalia
    , postSpråk
    , postUtdanning
    , putPersonalia
    , putSammendrag
    )

import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Cv.Cv as Cv exposing (Cv)
import Cv.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon)
import Cv.Sammendrag as Sammendrag exposing (Sammendrag)
import Cv.Spraakferdighet exposing (Spraakferdighet)
import Cv.Utdanning exposing (Utdanning)
import Feilmelding exposing (Feilmelding)
import Http exposing (..)
import Json.Decode
import Json.Encode
import Konsept exposing (Konsept)
import Personalia exposing (Personalia)
import Skjema.ArbeidserfaringSkjema
import Skjema.Fagdokumentasjon
import Skjema.Personalia
import Skjema.Sprak
import Skjema.Utdanning
import Sprakkoder exposing (Sprakkoder)
import Yrke as YrkeTypahead exposing (Yrke)


getPerson : (Result Error () -> msg) -> Cmd msg
getPerson msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person"
        , expect = expectWhatever msgConstructor
        }


postPerson : (Result Error () -> msg) -> Cmd msg
postPerson msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person"
        , expect = expectWhatever msgConstructor
        , body = emptyBody
        }


getPersonalia : (Result Error Personalia -> msg) -> Cmd msg
getPersonalia msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        }


postPersonalia : (Result Error Personalia -> msg) -> Cmd msg
postPersonalia msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = emptyBody
        }


putPersonalia : (Result Error Personalia -> msg) -> Skjema.Personalia.PersonaliaSkjema -> String -> Cmd msg
putPersonalia msgConstructor skjema id =
    put
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = Skjema.Personalia.encode skjema id |> jsonBody
        }


postSpråk : (Result Error (List Spraakferdighet) -> msg) -> Skjema.Sprak.SpråkSkjema -> Cmd msg
postSpråk msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list Cv.Spraakferdighet.decode)
        , body = Skjema.Sprak.encode skjema |> jsonBody
        }


getSpråkkoder : (Result Error (List Sprakkoder) -> msg) -> Cmd msg
getSpråkkoder msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/koder/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list Sprakkoder.decode)
        }


putSammendrag : (Result Error Sammendrag -> msg) -> String -> Cmd msg
putSammendrag msgConstructor sammendrag =
    put
        { url = "/cv-samtale/api/rest/cv/sammendrag"
        , expect = expectJson msgConstructor Sammendrag.decode
        , body = Json.Encode.object [ ( "sammendrag", Json.Encode.string sammendrag ) ] |> jsonBody
        }


getCv : (Result Error Cv -> msg) -> Cmd msg
getCv msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        }


postCv : (Result Error Cv -> msg) -> Cmd msg
postCv msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        , body = emptyBody
        }


getAAreg : (Result Error (List Arbeidserfaring) -> msg) -> Cmd msg
getAAreg msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv/aareg"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        }


postArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Skjema.ArbeidserfaringSkjema.ValidertArbeidserfaringSkjema -> Cmd msg
postArbeidserfaring msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/v2/arbeidserfaring"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        , body = Skjema.ArbeidserfaringSkjema.encode skjema |> jsonBody
        }


getYrkeTypeahead : (Result Error (List Yrke) -> msg) -> String -> Cmd msg
getYrkeTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/yrke?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list YrkeTypahead.decode)
        }


postUtdanning : (Result Error (List Utdanning) -> msg) -> Skjema.Utdanning.UtdanningSkjema -> Cmd msg
postUtdanning msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/utdanning"
        , expect = expectJson msgConstructor (Json.Decode.list Cv.Utdanning.decode)
        , body = Skjema.Utdanning.encode skjema "id" (Skjema.Utdanning.nuskode skjema) |> jsonBody
        }


postFagdokumentasjon : (Result Error (List Fagdokumentasjon) -> msg) -> Skjema.Fagdokumentasjon.FagdokumentasjonSkjema -> Cmd msg
postFagdokumentasjon msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/fagdokumentasjon"
        , expect = expectJson msgConstructor (Json.Decode.list Fagdokumentasjon.decode)
        , body = Skjema.Fagdokumentasjon.encode skjema "id" (Skjema.Fagdokumentasjon.fagdokumentasjonType skjema) |> jsonBody
        }


getFagbrevTypeahead : (Result Error (List Konsept) -> msg) -> String -> Cmd msg
getFagbrevTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/fagbrev?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list Konsept.decode)
        }


getMesterbrevTypeahead : (Result Error (List Konsept) -> msg) -> String -> Cmd msg
getMesterbrevTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/mesterbrev?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list Konsept.decode)
        }


getAutorisasjonTypeahead : (Result Error (List Konsept) -> msg) -> String -> Cmd msg
getAutorisasjonTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/autorisasjon?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list Konsept.decode)
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
