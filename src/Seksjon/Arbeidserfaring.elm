module Seksjon.Arbeidserfaring exposing (Model, Msg, SamtaleStatus(..), init, update, viewBrukerInput)

import Api
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import MeldingsLogg exposing (MeldingsLogg)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , arbeidserfaringListe : List Arbeidserfaring
    , aktivSamtale : Samtale
    }


arbeidserfaringListe : Model -> List Arbeidserfaring
arbeidserfaringListe (Model info) =
    info.arbeidserfaringListe


aktivSamtale : Model -> Samtale
aktivSamtale (Model info) =
    info.aktivSamtale


hentAAregArbeidserfaring : Model -> Cmd Msg
hentAAregArbeidserfaring (Model info) =
    Api.hentAAreg HentetAAregArbeidserfaring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )



--- UPDATE ---


type Msg
    = HentAAregArbeidserfaring
    | HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | BrukerOppdatererYrke YrkeInfo String
    | BrukerOppretterNyArbeidserfaring
    | BrukerVilRegistrerJobbTittel JobbtittelInfo
    | BrukerOppdatererJobbtittelFelt JobbtittelInfo String
    | BrukerVilRegistrereBedriftnavn BeriftnavnsInfo
    | BrukerOppdatererBedriftnavn BeriftnavnsInfo String


type Samtale
    = Intro
    | RegistrerYrke YrkeInfo
    | RegistrerNyTittel JobbtittelInfo
    | RegistrereBedriftNavn BeriftnavnsInfo


type alias YrkeInfo =
    { yrke : String }


type alias JobbtittelInfo =
    { yrke : YrkeInfo
    , jobbtittel : String
    }


type alias BeriftnavnsInfo =
    { yrke : YrkeInfo
    , jobbtittel : JobbtittelInfo
    , bedriftNavn : String
    }


type alias Lokasjon =
    { yrke : String
    , jobbtittel : String
    , bedriftNavn : String
    , lokasjon : String
    }


type alias Arbeidsoppgaver =
    { yrke : String
    , jobbtittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsOppgaver : String
    }


type alias Periode =
    { yrke : String
    , jobbtittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsOppgaver : String
    , fradato : String
    , tildato : String
    , navarende : Bool
    }


type Felt
    = YrkeFelt
    | JobbTittelFelt
    | BedriftNavnFelt
    | LokasjonFelt
    | ArbeidsOppgaverFelt
    | FradatoFelt
    | TildatoFelt
    | NavarendeFelt


yrkeInfoTilJobbtittelInfo : YrkeInfo -> JobbtittelInfo
yrkeInfoTilJobbtittelInfo yrke =
    { yrke = yrke, jobbtittel = "" }


jobbtittelInfoTilBedriftnavnsInfo : JobbtittelInfo -> BeriftnavnsInfo
jobbtittelInfoTilBedriftnavnsInfo jobbtittelInfo =
    { yrke = jobbtittelInfo.yrke, jobbtittel = jobbtittelInfo, bedriftNavn = "" }


update : Msg -> Model -> SamtaleStatus
update msg (Model info) =
    case msg of
        HentAAregArbeidserfaring ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        HentetAAregArbeidserfaring result ->
            case result of
                Ok arbeidserfaringFraAAreg ->
                    ( Model
                        { info
                            | arbeidserfaringListe =
                                arbeidserfaringFraAAreg ++ info.arbeidserfaringListe
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Err error ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerOppretterNyArbeidserfaring ->
            ( Model
                { info
                    | aktivSamtale =
                        RegistrerYrke { yrke = "" }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererYrke yrke string ->
            ( Model
                { info
                    | aktivSamtale = RegistrerYrke { yrke | yrke = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrerJobbTittel jobbtittelInfo ->
            ( Model
                { info | aktivSamtale = RegistrerNyTittel jobbtittelInfo }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererJobbtittelFelt jobbtittelInfo string ->
            ( Model
                { info
                    | aktivSamtale = RegistrerNyTittel { jobbtittelInfo | jobbtittel = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereBedriftnavn bedriftnavnInfo ->
            ( Model
                { info | aktivSamtale = RegistrereBedriftNavn bedriftnavnInfo }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererBedriftnavn beriftnavnsInfo string ->
            ( Model
                { info
                    | aktivSamtale = RegistrereBedriftNavn { beriftnavnsInfo | bedriftNavn = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model info) =
    case info.aktivSamtale of
        Intro ->
            div []
                [ button [ onClick BrukerOppretterNyArbeidserfaring ] [ text "Legg til arbeidserfaring" ]
                ]

        RegistrerYrke yrke ->
            div []
                [ label [] [ text "Yrke" ]
                , input
                    [ yrke.yrke |> value
                    , onInput (BrukerOppdatererYrke yrke)
                    ]
                    []
                , button
                    [ yrkeInfoTilJobbtittelInfo yrke
                        |> BrukerVilRegistrerJobbTittel
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        RegistrerNyTittel jobbtittelInfo ->
            div []
                [ label [] [ text "Skriv inn tittlen på jobben" ]
                , input
                    [ jobbtittelInfo.jobbtittel |> value
                    , onInput (BrukerOppdatererJobbtittelFelt jobbtittelInfo)
                    ]
                    []
                , button
                    [ jobbtittelInfoTilBedriftnavnsInfo jobbtittelInfo
                        |> BrukerVilRegistrereBedriftnavn
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        RegistrereBedriftNavn beriftnavnsInfo ->
            div []
                [ label [] [ text "Skriv inn navnet på bedriften du jobbet i" ]
                , input
                    [ beriftnavnsInfo.bedriftNavn |> value
                    , onInput (BrukerOppdatererBedriftnavn beriftnavnsInfo)
                    ]
                    []
                , button
                    []
                    [ text "Lagre" ]
                ]


init : MeldingsLogg -> Model
init gammelMeldingsLogg =
    Model
        { seksjonsMeldingsLogg = gammelMeldingsLogg
        , arbeidserfaringListe = []
        , aktivSamtale = Intro
        }
