module Seksjon.Utdanning exposing (Model, ModelInfo, Msg, Samtale(..), SamtaleStatus(..), historikk, init, update)

import Cv.Utdanning as Cv exposing (Utdanning)
import Personalia exposing (Personalia)
import Snakkeboble exposing (Snakkeboble(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { historikk : List Snakkeboble, aktivSamtale : Samtale, utdanning : List Utdanning }


type Samtale
    = BekreftOriginal


type Msg
    = UtdanningBekreftet


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Utdanning) (List Snakkeboble)



--- UPDATE ---


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        UtdanningBekreftet ->
            -- FIXME: her blir ikke historien oppdatert før den blir sendt videre
            Ferdig model.utdanning model.historikk


historikk : Model -> List Snakkeboble
historikk (Model model) =
    model.historikk ++ [ samtaleTilBoble model.aktivSamtale ]


nesteSamtaleSteg : ModelInfo -> String -> Samtale -> Model
nesteSamtaleSteg model tekst samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , historikk = model.historikk ++ [ samtaleTilBoble model.aktivSamtale, samtaleTilBoble samtaleSeksjon, Bruker tekst ]
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , historikk = model.historikk ++ [ samtaleTilBoble samtaleSeksjon ]
        }


samtaleTilBoble : Samtale -> Snakkeboble
samtaleTilBoble utdanningSeksjon =
    case utdanningSeksjon of
        BekreftOriginal ->
            Robot
                "Nå skal vi fylle ut utdanningen din."



--- INIT ---


init : List Utdanning -> Model
init utdanning =
    Model { historikk = [], aktivSamtale = BekreftOriginal, utdanning = utdanning }
