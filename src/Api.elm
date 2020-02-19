module Api exposing
    ( endreAnnenErfaring
    , endreArbeidserfaring
    , endrePersonalia
    , endreSammendrag
    , endreSertifikat
    , endreSynlighet
    , endreUtdanning
    , getAAreg
    , getAutorisasjonTypeahead
    , getCv
    , getFagbrevTypeahead
    , getHeaderInfo
    , getJobbprofil
    , getKompetanseJobbprofilTypeahead
    , getMesterbrevTypeahead
    , getOmradeJobbprofilTypeahead
    , getPerson
    , getPersonalia
    , getSertifikatTypeahead
    , getSpråkkoder
    , getYrkeJobbprofilTypeahead
    , getYrkeTypeahead
    , hentPoststed
    , logError
    , logErrorWithRequestBody
    , opprettAnnenErfaring
    , opprettArbeidserfaring
    , opprettCv
    , opprettFagdokumentasjon
    , opprettFørerkort
    , opprettKurs
    , opprettPerson
    , opprettPersonalia
    , opprettSertifikat
    , opprettSpråk
    , opprettUtdanning
    )

import AndreSider.HeaderInfo as HeaderInfo exposing (HeaderInfo)
import AnnenErfaring.AnnenErfaring as AnnenErfaring exposing (AnnenErfaring)
import AnnenErfaring.Skjema
import Arbeidserfaring.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Arbeidserfaring.Skjema
import Arbeidserfaring.Yrke as YrkeTypeahead exposing (Yrke)
import Cv exposing (Cv)
import Fagdokumentasjon.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon)
import Fagdokumentasjon.Konsept exposing (Konsept)
import Fagdokumentasjon.Skjema
import Feilmelding exposing (Feilmelding)
import Forerkort.Forerkort as Førerkort exposing (Førerkort)
import Forerkort.Skjema
import Http exposing (..)
import Jobbprofil.Jobbprofil as Jobbprofil exposing (Jobbprofil)
import Jobbprofil.Kompetanse as KompetanseTypeahead exposing (Kompetanse)
import Jobbprofil.Omrade as OmradeTypeahead exposing (Omrade)
import Json.Decode exposing (Decoder, bool, field)
import Json.Encode
import Kurs.Kurs as Kurs exposing (Kurs)
import Kurs.Skjema
import Person exposing (Person)
import Personalia.Personalia as Personalia exposing (Personalia)
import Personalia.Poststed exposing (Poststed)
import Personalia.Skjema
import Sammendrag exposing (Sammendrag)
import Sertifikat.Sertifikat as Sertifikat exposing (Sertifikat)
import Sertifikat.SertifikatTypeahead as SertifikatTypeahead exposing (SertifikatTypeahead)
import Sertifikat.Skjema
import Sprak.Skjema
import Sprak.Sprak as Språk exposing (Språk)
import Sprak.SprakKode as SpråkKode exposing (SpråkKode)
import Typeahead.Typeahead as Typeahead
import Url.Builder
import Utdanning.Skjema
import Utdanning.Utdanning as Utdanning exposing (Utdanning)


getPerson : (Result Error Person -> msg) -> Cmd msg
getPerson msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person"
        , expect = expectJson msgConstructor Person.decode
        }


opprettPerson : (Result Error Person -> msg) -> Cmd msg
opprettPerson msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person"
        , expect = expectJson msgConstructor Person.decode
        , body = emptyBody
        }


getHeaderInfo : (Result Error HeaderInfo -> msg) -> Cmd msg
getHeaderInfo msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person/headerinfo"
        , expect = expectJson msgConstructor HeaderInfo.decode
        }


endreSynlighet : (Result Error Bool -> msg) -> Bool -> Cmd msg
endreSynlighet msgConstructor synlighet =
    Http.post
        { url = "/cv-samtale/api/rest/person/synlighet"
        , expect = expectJson msgConstructor (field "synligForArbeidsgiver" bool)
        , body = Json.Encode.object [ ( "synligForArbeidsgiver", Json.Encode.bool synlighet ) ] |> jsonBody
        }


getPersonalia : (Result Error Personalia -> msg) -> Cmd msg
getPersonalia msgConstructor =
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


endrePersonalia : (Result Error Personalia -> msg) -> Personalia.Skjema.ValidertPersonaliaSkjema -> Cmd msg
endrePersonalia msgConstructor skjema =
    put
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body =
            Personalia.Skjema.encode skjema
                |> jsonBody
        }


opprettFørerkort : (Result Error (List Førerkort) -> msg) -> Forerkort.Skjema.ValidertFørerkortSkjema -> Cmd msg
opprettFørerkort msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/forerkort"
        , expect = expectJson msgConstructor (Json.Decode.list Førerkort.decode)
        , body = Forerkort.Skjema.encode skjema |> jsonBody
        }


opprettSpråk : (Result Error (List Språk) -> msg) -> Sprak.Skjema.SpråkSkjema -> Cmd msg
opprettSpråk msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list Språk.decode)
        , body = Sprak.Skjema.encode skjema |> jsonBody
        }


getSpråkkoder : (Result Error (List SpråkKode) -> msg) -> Cmd msg
getSpråkkoder msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/koder/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list SpråkKode.decode)
        }


hentPoststed : (Result Error Poststed -> msg) -> String -> Cmd msg
hentPoststed msgConstructor postnummer =
    Http.get
        { url = "/cv-samtale/api/rest/koder/poststed?postnummer=" ++ postnummer
        , expect = expectJson msgConstructor Personalia.Poststed.decode
        }


endreSammendrag : (Result Error Sammendrag -> msg) -> String -> Cmd msg
endreSammendrag msgConstructor sammendrag =
    put
        { url = "/cv-samtale/api/rest/cv/sammendrag"
        , expect = expectJson msgConstructor Sammendrag.decode
        , body =
            sammendrag
                |> Sammendrag.encodeSammendrag
                |> jsonBody
        }


getCv : (Result Error Cv -> msg) -> Cmd msg
getCv msgConstructor =
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


getJobbprofil : (Result Error Jobbprofil -> msg) -> Cmd msg
getJobbprofil msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/jobbprofil"
        , expect = expectJson msgConstructor Jobbprofil.decode
        }


getAAreg : (Result Error (List Arbeidserfaring) -> msg) -> Cmd msg
getAAreg msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv/aareg"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        }


opprettAnnenErfaring : (Result Error (List AnnenErfaring) -> msg) -> AnnenErfaring.Skjema.ValidertAnnenErfaringSkjema -> Cmd msg
opprettAnnenErfaring msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/annenerfaring"
        , expect = expectJson msgConstructor (Json.Decode.list AnnenErfaring.decode)
        , body = AnnenErfaring.Skjema.encode skjema |> jsonBody
        }


endreAnnenErfaring : (Result Error (List AnnenErfaring) -> msg) -> AnnenErfaring.Skjema.ValidertAnnenErfaringSkjema -> String -> Cmd msg
endreAnnenErfaring msgConstructor skjema id =
    put
        { url = "/cv-samtale/api/rest/cv/annenerfaring/" ++ id
        , expect = expectJson msgConstructor (Json.Decode.list AnnenErfaring.decode)
        , body = AnnenErfaring.Skjema.encode skjema |> jsonBody
        }


opprettArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Arbeidserfaring.Skjema.ValidertArbeidserfaringSkjema -> Cmd msg
opprettArbeidserfaring msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/v2/arbeidserfaring"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        , body = Arbeidserfaring.Skjema.encode skjema |> jsonBody
        }


endreArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Arbeidserfaring.Skjema.ValidertArbeidserfaringSkjema -> String -> Cmd msg
endreArbeidserfaring msgConstructor skjema id =
    put
        { url = "/cv-samtale/api/rest/cv/v2/arbeidserfaring/" ++ id
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        , body = Arbeidserfaring.Skjema.encode skjema |> jsonBody
        }


opprettKurs : (Result Error (List Kurs) -> msg) -> Kurs.Skjema.ValidertKursSkjema -> Cmd msg
opprettKurs msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/kurs"
        , expect = expectJson msgConstructor (Json.Decode.list Kurs.decode)
        , body = Kurs.Skjema.encode skjema |> jsonBody
        }


opprettSertifikat : (Result Error (List Sertifikat) -> msg) -> Sertifikat.Skjema.ValidertSertifikatSkjema -> Cmd msg
opprettSertifikat msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/sertifikat"
        , expect = expectJson msgConstructor (Json.Decode.list Sertifikat.decode)
        , body = Sertifikat.Skjema.encode skjema |> jsonBody
        }


endreSertifikat : (Result Error (List Sertifikat) -> msg) -> Sertifikat.Skjema.ValidertSertifikatSkjema -> String -> Cmd msg
endreSertifikat msgConstructor skjema id =
    put
        { url = "/cv-samtale/api/rest/cv/sertifikat/" ++ id
        , expect = expectJson msgConstructor (Json.Decode.list Sertifikat.decode)
        , body = Sertifikat.Skjema.encode skjema |> jsonBody
        }


getYrkeTypeahead : (Typeahead.Query -> Result Error (List Yrke) -> msg) -> Typeahead.Query -> Cmd msg
getYrkeTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/yrke" ++ Url.Builder.toQuery [ Url.Builder.string "q" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list YrkeTypeahead.decode)
        }


getYrkeJobbprofilTypeahead : (Typeahead.Query -> Result Error (List Yrke) -> msg) -> Typeahead.Query -> Cmd msg
getYrkeJobbprofilTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/stilling" ++ Url.Builder.toQuery [ Url.Builder.string "query" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list YrkeTypeahead.decode)
        }


getOmradeJobbprofilTypeahead : (Typeahead.Query -> Result Error (List Omrade) -> msg) -> Typeahead.Query -> Cmd msg
getOmradeJobbprofilTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/sted" ++ Url.Builder.toQuery [ Url.Builder.string "query" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list OmradeTypeahead.decode)
        }


getKompetanseJobbprofilTypeahead : (Typeahead.Query -> Result Error (List Kompetanse) -> msg) -> Typeahead.Query -> Cmd msg
getKompetanseJobbprofilTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/kompetanse" ++ Url.Builder.toQuery [ Url.Builder.string "query" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list KompetanseTypeahead.decode)
        }


opprettUtdanning : (Result Error (List Utdanning) -> msg) -> Utdanning.Skjema.ValidertUtdanningSkjema -> Cmd msg
opprettUtdanning msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/utdanning"
        , expect = expectJson msgConstructor (Json.Decode.list Utdanning.decode)
        , body = Utdanning.Skjema.encode skjema |> jsonBody
        }


endreUtdanning : (Result Error (List Utdanning) -> msg) -> Utdanning.Skjema.ValidertUtdanningSkjema -> String -> Cmd msg
endreUtdanning msgConstructor skjema id =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/cv-samtale/api/rest/cv/utdanning/" ++ id
        , expect = expectJson msgConstructor (Json.Decode.list Utdanning.decode)
        , body =
            skjema
                |> Utdanning.Skjema.encode
                |> jsonBody
        , timeout = Nothing
        , tracker = Nothing
        }


opprettFagdokumentasjon : (Result Error (List Fagdokumentasjon) -> msg) -> Fagdokumentasjon.Skjema.ValidertFagdokumentasjonSkjema -> Cmd msg
opprettFagdokumentasjon msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/fagdokumentasjon"
        , expect = expectJson msgConstructor (Json.Decode.list Fagdokumentasjon.decode)
        , body =
            skjema
                |> Fagdokumentasjon.Skjema.encode
                |> jsonBody
        }


getFagbrevTypeahead : (Typeahead.Query -> Result Error (List Konsept) -> msg) -> Typeahead.Query -> Cmd msg
getFagbrevTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/fagbrev" ++ Url.Builder.toQuery [ Url.Builder.string "q" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list Fagdokumentasjon.Konsept.decode)
        }


getMesterbrevTypeahead : (Typeahead.Query -> Result Error (List Konsept) -> msg) -> Typeahead.Query -> Cmd msg
getMesterbrevTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/mesterbrev" ++ Url.Builder.toQuery [ Url.Builder.string "q" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list Fagdokumentasjon.Konsept.decode)
        }


getAutorisasjonTypeahead : (Typeahead.Query -> Result Error (List Konsept) -> msg) -> Typeahead.Query -> Cmd msg
getAutorisasjonTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/autorisasjoner" ++ Url.Builder.toQuery [ Url.Builder.string "q" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list Fagdokumentasjon.Konsept.decode)
        }


getSertifikatTypeahead : (Typeahead.Query -> Result Error (List SertifikatTypeahead) -> msg) -> Typeahead.Query -> Cmd msg
getSertifikatTypeahead msgConstructor query =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/autorisasjon" ++ Url.Builder.toQuery [ Url.Builder.string "q" (Typeahead.queryToString query) ]
        , expect = expectJson (msgConstructor query) (Json.Decode.list SertifikatTypeahead.decode)
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


logErrorWithRequestBody : msg -> String -> Http.Error -> Json.Encode.Value -> Cmd msg
logErrorWithRequestBody msg operation error requestBody =
    Feilmelding.feilmelding operation error
        |> Maybe.map (Feilmelding.withRequestBody requestBody)
        |> Maybe.map (logError (always msg))
        |> Maybe.withDefault Cmd.none


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
