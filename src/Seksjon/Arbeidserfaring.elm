module Seksjon.Arbeidserfaring exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Api
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Melding exposing (Melding)
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


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )



--- UPDATE ---


type Msg
    = BrukerOppretterNyArbeidserfaring
    | HentAAregArbeidserfaring
    | HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | BrukerOppdatererYrke String
    | BrukerVilRegistrerJobbTittel JobbtittelInfo
    | BrukerOppdatererJobbtittelFelt JobbtittelInfo String
    | BrukerVilRegistrereBedriftnavn BeriftnavnsInfo
    | BrukerOppdatererBedriftnavn BeriftnavnsInfo String
    | BrukerVilRegistrereLokasjon LokasjonInfo
    | BrukerOppdatererLokasjon LokasjonInfo String
    | BrukerVilRegistrereArbeidsoppgaver ArbeidsoppgaverInfo
    | BrukerOppdatererArbeidsoppgaver ArbeidsoppgaverInfo String
    | BrukerVilRegistrerePeriode PeriodeInfo
    | BrukerOppdatererFradato PeriodeInfo String
    | BrukerOppdatererTildato PeriodeInfo String
    | BrukerOppdatererNaavarende PeriodeInfo


type Samtale
    = Intro
    | RegistrerYrke YrkeInfo
    | RegistrerNyTittel JobbtittelInfo
    | RegistrereBedriftNavn BeriftnavnsInfo
    | RegistrereLokasjon LokasjonInfo
    | RegistrereArbeidsoppgaver ArbeidsoppgaverInfo
    | RegistrerePeriodeGammelJobb PeriodeInfo
    | RegistrerePeriodeNaavarendeJobb PeriodeInfo


type alias YrkeInfo =
    { yrke : String }


type alias JobbtittelInfo =
    { tidligereInfo : YrkeInfo
    , jobbtittel : String
    }


type alias BeriftnavnsInfo =
    { tidligereInfo : JobbtittelInfo
    , bedriftNavn : String
    }


type alias LokasjonInfo =
    { tidligereInfo : BeriftnavnsInfo
    , lokasjon : String
    }


type alias ArbeidsoppgaverInfo =
    { tidligereInfo : LokasjonInfo
    , arbeidsoppgaver : String
    }


type alias PeriodeInfo =
    { tidligereInfo : ArbeidsoppgaverInfo
    , fradato : String
    , tildato : String
    , naavarende : Bool
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
    { tidligereInfo = yrke, jobbtittel = "" }


jobbtittelInfoTilBedriftnavnsInfo : JobbtittelInfo -> BeriftnavnsInfo
jobbtittelInfoTilBedriftnavnsInfo jobbtittelInfo =
    { tidligereInfo = jobbtittelInfo
    , bedriftNavn = ""
    }


bedriftnavnsInfoTilLokasjonInfo : BeriftnavnsInfo -> LokasjonInfo
bedriftnavnsInfoTilLokasjonInfo beriftnavnsInfo =
    { tidligereInfo = beriftnavnsInfo
    , lokasjon = ""
    }


lokasjonInfoTilArbeidsoppgaverInfo : LokasjonInfo -> ArbeidsoppgaverInfo
lokasjonInfoTilArbeidsoppgaverInfo lokasjonInfo =
    { tidligereInfo = lokasjonInfo
    , arbeidsoppgaver = ""
    }


arbeidsoppgaverInfoTilPeriodeInfo : ArbeidsoppgaverInfo -> PeriodeInfo
arbeidsoppgaverInfoTilPeriodeInfo arbeidsoppgaverInfo =
    { tidligereInfo = arbeidsoppgaverInfo
    , fradato = ""
    , tildato = ""
    , naavarende = False
    }


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
            ( RegistrerYrke { yrke = "" }
                |> nesteSamtaleSteg info (Melding.svar [ "Legg til arbeidserfaring" ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererYrke string ->
            ( Model
                { info
                    | aktivSamtale = RegistrerYrke { yrke = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrerJobbTittel jobbtittelInfo ->
            ( RegistrerNyTittel jobbtittelInfo
                |> nesteSamtaleSteg info (Melding.svar [ jobbtittelInfo.tidligereInfo.yrke ])
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
            ( RegistrereBedriftNavn bedriftnavnInfo
                |> nesteSamtaleSteg info (Melding.svar [ bedriftnavnInfo.tidligereInfo.jobbtittel ])
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

        BrukerVilRegistrereLokasjon lokasjonInfo ->
            ( RegistrereLokasjon lokasjonInfo
                |> nesteSamtaleSteg info (Melding.svar [ lokasjonInfo.lokasjon ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererLokasjon lokasjonsInfo string ->
            ( Model
                { info
                    | aktivSamtale = RegistrereLokasjon { lokasjonsInfo | lokasjon = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            ( RegistrereArbeidsoppgaver arbeidsoppgaverInfo
                |> nesteSamtaleSteg info (Melding.svar [ arbeidsoppgaverInfo.arbeidsoppgaver ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererArbeidsoppgaver arbeidsoppgaverInfo string ->
            ( Model
                { info
                    | aktivSamtale =
                        RegistrereArbeidsoppgaver { arbeidsoppgaverInfo | arbeidsoppgaver = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrerePeriode periodeInfo ->
            ( RegistrerePeriodeGammelJobb periodeInfo
                |> nesteSamtaleSteg info (Melding.svar [ periodeInfo.tidligereInfo.arbeidsoppgaver ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererFradato periodeInfo string ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        BrukerOppdatererTildato periodeInfo string ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        BrukerOppdatererNaavarende periodeInfo ->
            ( Model
                { info
                    | aktivSamtale =
                        periodeInfo
                            |> toggleNaavarende
                            |> setRiktigSamtalesteg
                }
            , Cmd.none
            )
                |> IkkeFerdig


toggleNaavarende : PeriodeInfo -> PeriodeInfo
toggleNaavarende periodeInfo =
    if periodeInfo.naavarende then
        { periodeInfo | naavarende = False }

    else
        { periodeInfo | naavarende = True }


setRiktigSamtalesteg : PeriodeInfo -> Samtale
setRiktigSamtalesteg periodeInfo =
    if periodeInfo.naavarende then
        RegistrerePeriodeNaavarendeJobb periodeInfo

    else
        RegistrerePeriodeGammelJobb periodeInfo


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg modelInfo melding samtaleSeksjon =
    Model
        { modelInfo
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                modelInfo.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg personaliaSeksjon =
    case personaliaSeksjon of
        Intro ->
            [ Melding.spørsmål [ "Nå skal vi registrere arbeidserfaringen din" ] ]

        RegistrerYrke yrkeInfo ->
            [ Melding.spørsmål
                [ "Vi begynner med å registrere yrke,"
                , "det er viktig at du velger et yrke fra listen."
                , "Hvis ikke navnet passer helt, så kan du endre det senere."
                ]
            ]

        RegistrerNyTittel jobbtittelInfo ->
            [ Melding.spørsmål [ "Hva var tittelen på jobben? Her kan du skrive det du vil at skal stå på CVen" ] ]

        RegistrereBedriftNavn beriftnavnsInfo ->
            [ Melding.spørsmål [ "Hva var bedriften/arbeidsplassens navn?" ] ]

        RegistrereLokasjon lokasjonInfo ->
            [ Melding.spørsmål [ "Hvor befant bedriften seg?" ] ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            [ Melding.spørsmål
                [ "Flott! Nå kan du skrive litt utfyllende om jobben."
                , "For eksempel hvilke arbeidsoppgaver du hadde, det viktigste du lærte osv"
                ]
            ]

        RegistrerePeriodeGammelJobb periodeInfo ->
            [ Melding.spørsmål [ "Hvilken periode jobbet du der?" ] ]

        RegistrerePeriodeNaavarendeJobb periodeInfo ->
            [ Melding.spørsmål [ "Når startet du i denne jobben?" ] ]



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
                    , onInput BrukerOppdatererYrke
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
                    [ bedriftnavnsInfoTilLokasjonInfo beriftnavnsInfo
                        |> BrukerVilRegistrereLokasjon
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        RegistrereLokasjon lokasjonInfo ->
            div []
                [ label [] [ text "Skriv inn navnet på bedriften du jobbet i" ]
                , input
                    [ lokasjonInfo.lokasjon |> value
                    , lokasjonInfo
                        |> BrukerOppdatererLokasjon
                        |> onInput
                    ]
                    []
                , button
                    [ lokasjonInfoTilArbeidsoppgaverInfo lokasjonInfo
                        |> BrukerVilRegistrereArbeidsoppgaver
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            div []
                [ label [] [ text "Utfyllende text" ]
                , input
                    [ arbeidsoppgaverInfo.arbeidsoppgaver |> value
                    , arbeidsoppgaverInfo
                        |> BrukerOppdatererArbeidsoppgaver
                        |> onInput
                    ]
                    []
                , button
                    [ arbeidsoppgaverInfoTilPeriodeInfo arbeidsoppgaverInfo
                        |> BrukerVilRegistrerePeriode
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        RegistrerePeriodeGammelJobb periodeInfo ->
            div []
                [ label [] [ text "Fradato" ]
                , input
                    [ periodeInfo.fradato |> value
                    , periodeInfo
                        |> BrukerOppdatererFradato
                        |> onInput
                    ]
                    []
                , label [] [ text "Tildato" ]
                , input
                    [ periodeInfo.tildato |> value
                    , periodeInfo
                        |> BrukerOppdatererTildato
                        |> onInput
                    ]
                    []
                , input
                    [ type_ "checkbox"
                    , BrukerOppdatererNaavarende periodeInfo
                        |> onClick
                    ]
                    []
                ]

        RegistrerePeriodeNaavarendeJobb periodeInfo ->
            div []
                [ label [] [ text "Fradato" ]
                , input
                    [ periodeInfo.fradato |> value
                    , periodeInfo
                        |> BrukerOppdatererFradato
                        |> onInput
                    ]
                    []
                , input
                    [ type_ "checkbox"
                    , BrukerOppdatererNaavarende periodeInfo
                        |> onClick
                    ]
                    []
                ]


init : MeldingsLogg -> Model
init gammelMeldingsLogg =
    Model
        { seksjonsMeldingsLogg = gammelMeldingsLogg
        , arbeidserfaringListe = []
        , aktivSamtale = Intro
        }
