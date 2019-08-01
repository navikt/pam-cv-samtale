module Api exposing
    ( getAAreg
    , getCv
    , getPerson
    , getPersonalia
    , getSpråkkoder
    , getYrkeTypeahead
    , logError
    , postArbeidserfaring
    , postCv
    , postPerson
    , postPersonalia
    , postSpråk
    , postSynlighet
    , postUtdanning
    , putPersonalia
    , putSammendrag
    )

import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Cv.Cv as Cv exposing (Cv)
import Cv.Sammendrag as Sammendrag exposing (Sammendrag)
import Cv.Spraakferdighet exposing (Spraakferdighet)
import Cv.Utdanning exposing (Utdanning)
import Feilmelding exposing (Feilmelding)
import Http exposing (..)
import Json.Decode exposing (Decoder, bool, field, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Person exposing (Person)
import Personalia exposing (Personalia)
import Skjema.ArbeidserfaringSkjema
import Skjema.Personalia
import Skjema.Sprak
import Skjema.Utdanning
import Sprakkoder exposing (Sprakkoder)
import Yrke as YrkeTypahead exposing (Yrke)


getPerson : (Result Error Person -> msg) -> Cmd msg
getPerson msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person"
        , expect = expectJson msgConstructor Person.decode
        }


postPerson : (Result Error () -> msg) -> Cmd msg
postPerson msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person"
        , expect = expectWhatever msgConstructor
        , body = emptyBody
        }


postSynlighet : (Result Error Bool -> msg) -> Bool -> Cmd msg
postSynlighet msgConstructor synlighet =
    Http.post
        { url = "/cv-samtale/api/rest/person/synlighet"
        , expect = expectJson msgConstructor (field "cvSynligForArbeidsgiver" bool)
        , body = Json.Encode.object [ ( "synligForArbeidsgiver", Json.Encode.bool synlighet ) ] |> jsonBody
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
