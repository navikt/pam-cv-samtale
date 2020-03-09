module Jobbprofil.JobbprofilValg exposing (..)

import Json.Decode exposing (..)
import Json.Encode



--- OMFANG ---


type Omfang
    = Heltid
    | Deltid


omfangValg : List Omfang
omfangValg =
    [ Heltid
    , Deltid
    ]


omfangTilBackendString : Omfang -> String
omfangTilBackendString omfang =
    case omfang of
        Heltid ->
            "HELTID"

        Deltid ->
            "DELTID"


omfangLabel : Omfang -> String
omfangLabel omfang =
    case omfang of
        Heltid ->
            "Heltid"

        Deltid ->
            "Deltid"


decodeOmfang : Decoder Omfang
decodeOmfang =
    Json.Decode.string
        |> Json.Decode.andThen decodeOmfangString


decodeOmfangString : String -> Decoder Omfang
decodeOmfangString omfang =
    if omfang == "HELTID" then
        succeed Heltid

    else if omfang == "DELTID" then
        succeed Deltid

    else
        fail ("Decoding av enum omfang feilet. Klarer ikke decode verdi: " ++ omfang)



--- ANSETTELSESFORM ---


type AnsettelsesForm
    = Fast
    | Vikariat
    | Engasjement
    | Prosjekt
    | Sesong
    | Trainee
    | Lærling
    | SelvstendigNæringsdrivende
    | Feriejobb
    | Annet


ansettelsesformValg : List AnsettelsesForm
ansettelsesformValg =
    [ Fast
    , Vikariat
    , Engasjement
    , Prosjekt
    , Sesong
    , Trainee
    , Lærling
    , SelvstendigNæringsdrivende
    , Feriejobb
    , Annet
    ]


ansettelsesFormTilBackendString : AnsettelsesForm -> String
ansettelsesFormTilBackendString af =
    case af of
        Fast ->
            "FAST"

        Vikariat ->
            "VIKARIAT"

        Engasjement ->
            "ENGASJEMENT"

        Prosjekt ->
            "PROSJEKT"

        Sesong ->
            "SESONG"

        Trainee ->
            "TRAINEE"

        Lærling ->
            "LAERLING"

        SelvstendigNæringsdrivende ->
            "SELVSTENDIG_NAERINGSDRIVENDE"

        Feriejobb ->
            "FERIEJOBB"

        Annet ->
            "ANNET"


ansettelsesFormLabel : AnsettelsesForm -> String
ansettelsesFormLabel af =
    case af of
        Fast ->
            "Fast"

        Vikariat ->
            "Vikariat"

        Engasjement ->
            "Engasjement"

        Prosjekt ->
            "Prosjekt"

        Sesong ->
            "Sesong"

        Trainee ->
            "Trainee"

        Lærling ->
            "Lærling"

        SelvstendigNæringsdrivende ->
            "Selvstendig næringsdrivende"

        Feriejobb ->
            "Feriejobb"

        Annet ->
            "Annet"


decodeAnsettelsesform : Decoder AnsettelsesForm
decodeAnsettelsesform =
    Json.Decode.string
        |> Json.Decode.andThen decodeAnsettelsesformString


decodeAnsettelsesformString : String -> Decoder AnsettelsesForm
decodeAnsettelsesformString af =
    if af == "FAST" then
        succeed Fast

    else if af == "VIKARIAT" then
        succeed Vikariat

    else if af == "ENGASJEMENT" then
        succeed Engasjement

    else if af == "PROSJEKT" then
        succeed Prosjekt

    else if af == "SESONG" then
        succeed Sesong

    else if af == "TRAINEE" then
        succeed Trainee

    else if af == "LAERLING" then
        succeed Lærling

    else if af == "SELVSTENDIG_NAERINGSDRIVENDE" then
        succeed SelvstendigNæringsdrivende

    else if af == "FERIEJOBB" then
        succeed Feriejobb

    else if af == "ANNET" then
        succeed Annet

    else
        fail ("Decoding av enum Ansettelsesform feilet. Klarer ikke decode verdi: " ++ af)



--- OPPSTART ---


type Oppstart
    = LedigNå
    | OmTreMåneder
    | EtterAvtale


oppstartValg : List Oppstart
oppstartValg =
    [ LedigNå
    , OmTreMåneder
    , EtterAvtale
    ]


oppstartTilBackendString : Oppstart -> String
oppstartTilBackendString oppstart =
    case oppstart of
        LedigNå ->
            "LEDIG_NAA"

        OmTreMåneder ->
            "ETTER_TRE_MND"

        EtterAvtale ->
            "ETTER_AVTALE"


oppstartLabel : Oppstart -> String
oppstartLabel oppstart =
    case oppstart of
        LedigNå ->
            "Jeg kan begynne nå"

        OmTreMåneder ->
            "Jeg har 3 måneder oppsigelse"

        EtterAvtale ->
            "Jeg kan begynne etter nærmere avtale "


decodeOppstart : Decoder Oppstart
decodeOppstart =
    Json.Decode.string
        |> Json.Decode.andThen decodeOppstartString


decodeOppstartString : String -> Decoder Oppstart
decodeOppstartString oppstart =
    if oppstart == "LEDIG_NAA" then
        succeed LedigNå

    else if oppstart == "ETTER_TRE_MND" then
        succeed OmTreMåneder

    else if oppstart == "ETTER_AVTALE" then
        succeed EtterAvtale

    else
        fail ("Decoding av enum Oppstart feilet. Klarer ikke decode verdi: " ++ oppstart)



--- TIDSPUNKT---


type Arbeidstidspunkt
    = Dag
    | Kveld
    | Natt


arbeidstidspunktTilBackendString : Arbeidstidspunkt -> String
arbeidstidspunktTilBackendString tid =
    case tid of
        Dag ->
            "DAGTID"

        Kveld ->
            "KVELD"

        Natt ->
            "NATT"



--- DAG ---


type Arbeidsdag
    = Lørdag
    | Søndag


arbeidsdagTilBackendString : Arbeidsdag -> String
arbeidsdagTilBackendString dag =
    case dag of
        Lørdag ->
            "LOERDAG"

        Søndag ->
            "SOENDAG"



--- ORDNING ---


type ArbeidstidOrdning
    = Vakt
    | Skift
    | Turnus


arbeidstidOrdningTilBackendString : ArbeidstidOrdning -> String
arbeidstidOrdningTilBackendString ordning =
    case ordning of
        Vakt ->
            "VAKT"

        Skift ->
            "SKIFT"

        Turnus ->
            "TURNUS"



---- ARBEIDSTIDER ---


type Arbeidstider
    = Arbeidstid Arbeidstidspunkt
    | Arbeidsdager Arbeidsdag
    | ArbeidstidOrdning ArbeidstidOrdning


arbeidstidValg : List Arbeidstider
arbeidstidValg =
    [ Arbeidstid Dag
    , Arbeidstid Kveld
    , Arbeidstid Natt
    , Arbeidsdager Lørdag
    , Arbeidsdager Søndag
    , ArbeidstidOrdning Turnus
    , ArbeidstidOrdning Skift
    , ArbeidstidOrdning Vakt
    ]


arbeidstidLabel : Arbeidstider -> String
arbeidstidLabel tid =
    case tid of
        Arbeidstid Dag ->
            "Dag"

        Arbeidstid Kveld ->
            "Kveld"

        Arbeidstid Natt ->
            "Natt"

        Arbeidsdager Lørdag ->
            "Lørdag"

        Arbeidsdager Søndag ->
            "Søndag"

        ArbeidstidOrdning Turnus ->
            "Turnus"

        ArbeidstidOrdning Skift ->
            "Skift"

        ArbeidstidOrdning Vakt ->
            "Vakt"


decodeArbeidstider : Decoder Arbeidstider
decodeArbeidstider =
    Json.Decode.string
        |> Json.Decode.andThen decodeArbeidstiderString


decodeArbeidstiderString : String -> Decoder Arbeidstider
decodeArbeidstiderString tid =
    if tid == "LOERDAG" then
        succeed (Arbeidsdager Lørdag)

    else if tid == "SOENDAG" then
        succeed (Arbeidsdager Søndag)

    else if tid == "DAGTID" then
        succeed (Arbeidstid Dag)

    else if tid == "KVELD" then
        succeed (Arbeidstid Kveld)

    else if tid == "NATT" then
        succeed (Arbeidstid Natt)

    else if tid == "SKIFT" then
        succeed (ArbeidstidOrdning Skift)

    else if tid == "VAKT" then
        succeed (ArbeidstidOrdning Vakt)

    else if tid == "TURNUS" then
        succeed (ArbeidstidOrdning Turnus)

    else
        fail ("Decoding av enum Arbeitider feilet. Klarer ikke decode verdi: " ++ tid)


encodeArbeidstider : List Arbeidstider -> List ( String, Json.Encode.Value )
encodeArbeidstider arbeidstider =
    let
        arbeidstidspunkt =
            List.filterMap
                (\it ->
                    case it of
                        Arbeidstid value ->
                            Just value

                        _ ->
                            Nothing
                )
                arbeidstider

        arbeidsdager =
            List.filterMap
                (\it ->
                    case it of
                        Arbeidsdager value ->
                            Just value

                        _ ->
                            Nothing
                )
                arbeidstider

        arbeidtidsordning =
            List.filterMap
                (\it ->
                    case it of
                        ArbeidstidOrdning value ->
                            Just value

                        _ ->
                            Nothing
                )
                arbeidstider
    in
    [ ( "arbeidsdagerliste", Json.Encode.list Json.Encode.string (List.map arbeidsdagTilBackendString arbeidsdager) )
    , ( "arbeidstidsordningliste", Json.Encode.list Json.Encode.string (List.map arbeidstidOrdningTilBackendString arbeidtidsordning) )
    ]


encodeArbeidspunkt : List Arbeidstider -> List ( String, Json.Encode.Value )
encodeArbeidspunkt arbeidstider =
    let
        arbeidstidspunkt =
            List.filterMap
                (\it ->
                    case it of
                        Arbeidstid value ->
                            Just value

                        _ ->
                            Nothing
                )
                arbeidstider
    in
    [ ( "arbeidstidliste", Json.Encode.list Json.Encode.string (List.map arbeidstidspunktTilBackendString arbeidstidspunkt) )
    ]
