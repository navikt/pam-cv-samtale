module Skjema.Sertifikat exposing
    ( SertifikatSkjema
    , TypeaheadFelt(..)
    , ValidertSertifikatSkjema
    , encode
    , initValidertSkjema
    , mapTypeaheadState
    , oppdaterSertifikatFelt
    , tilUvalidertSkjema
    , velgAktivtSertifikatITypeahead
    )

import Cv.Sertifikat exposing (Sertifikat)
import Dato exposing (Måned(..), År)
import Json.Encode
import SertifikatFelt exposing (SertifikatFelt)
import TypeaheadState exposing (TypeaheadState)


type SertifikatSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type ValidertSertifikatSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias UvalidertSkjemaInfo =
    { sertifikatFelt : TypeaheadFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    , utløperMåned : Måned
    , utløperÅr : År
    }


type alias ValidertSkjemaInfo =
    { sertifikatFelt : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    , utløperMåned : Måned
    , utløperÅr : År
    }


type TypeaheadFelt
    = SertifikatFelt SertifikatFelt
    | Typeahead (TypeaheadState SertifikatFelt)



--INIT --


initValidertSkjema : ValidertSkjemaInfo -> ValidertSertifikatSkjema
initValidertSkjema skjema =
    ValidertSkjema skjema



--- INNHOLD ---


type Felt
    = Utsteder


innholdTekstFelt : Felt -> SertifikatSkjema -> String
innholdTekstFelt felt (UvalidertSkjema skjema) =
    case felt of
        Utsteder ->
            skjema.utsteder


utsteder : SertifikatSkjema -> String
utsteder (UvalidertSkjema skjema) =
    skjema.utsteder


sertifikatTypeahed : SertifikatSkjema -> TypeaheadFelt
sertifikatTypeahed (UvalidertSkjema skjema) =
    skjema.sertifikatFelt


fullførtMåned : SertifikatSkjema -> Måned
fullførtMåned (UvalidertSkjema skjema) =
    skjema.fullførtMåned


fullførtÅr : SertifikatSkjema -> År
fullførtÅr (UvalidertSkjema skjema) =
    skjema.fullførtÅr


utløperMåned : SertifikatSkjema -> Måned
utløperMåned (UvalidertSkjema skjema) =
    skjema.utløperMåned


utløperÅr : SertifikatSkjema -> År
utløperÅr (UvalidertSkjema skjema) =
    skjema.utløperÅr



--- OPPDATERING ---


oppdaterSertifikatFelt : SertifikatSkjema -> String -> SertifikatSkjema
oppdaterSertifikatFelt (UvalidertSkjema skjema) feltInnhold =
    case skjema.sertifikatFelt of
        SertifikatFelt _ ->
            UvalidertSkjema
                { skjema
                    | sertifikatFelt =
                        TypeaheadState.init feltInnhold
                            |> Typeahead
                }

        Typeahead typeaheadState ->
            UvalidertSkjema
                { skjema
                    | sertifikatFelt =
                        typeaheadState
                            |> TypeaheadState.updateValue feltInnhold
                            |> Typeahead
                }


mapTypeaheadState : SertifikatSkjema -> (TypeaheadState SertifikatFelt -> TypeaheadState SertifikatFelt) -> SertifikatSkjema
mapTypeaheadState (UvalidertSkjema skjema) funksjon =
    case skjema.sertifikatFelt of
        SertifikatFelt _ ->
            UvalidertSkjema skjema

        Typeahead typeaheadState ->
            UvalidertSkjema
                { skjema
                    | sertifikatFelt =
                        typeaheadState
                            |> funksjon
                            |> Typeahead
                }


velgAktivtSertifikatITypeahead : SertifikatSkjema -> SertifikatSkjema
velgAktivtSertifikatITypeahead (UvalidertSkjema skjema) =
    case skjema.sertifikatFelt of
        SertifikatFelt _ ->
            UvalidertSkjema skjema

        Typeahead typeaheadState ->
            case TypeaheadState.getActive typeaheadState of
                Just active ->
                    UvalidertSkjema { skjema | sertifikatFelt = SertifikatFelt active }

                Nothing ->
                    UvalidertSkjema skjema


oppdaterUtsteder : String -> SertifikatSkjema -> SertifikatSkjema
oppdaterUtsteder oppdatering (UvalidertSkjema skjema) =
    UvalidertSkjema { skjema | utsteder = oppdatering }



--- VALIDERING ---
{-

   validertSkjema : SertifikatSkjema -> Maybe ValidertSertifikatSkjema
   validertSkjema (UvalidertSkjema skjema) =
       case skjema.sertifikatFelt of
           SertifikatFelt sertifikatfelt ->
               if SertifikatFelt.label sertifikatfelt /= "" then
                   Maybe.map2
                       (\_ ->
                           ValidertSkjema
                               { sertifikatFelt = sertifikatfelt
                               , utsteder = skjema.utsteder
                               , fullførtMåned = skjema.fullførtMåned
                               , fullførtÅr = skjema.fullførtÅr
                               , utløperMåned = skjema.utløperMåned
                               , utløperÅr = skjema.utløperÅr
                               }
                       )

               else
                   Nothing

           Typeahead _ ->
               Nothing


-}


tilUvalidertSkjema : ValidertSertifikatSkjema -> SertifikatSkjema
tilUvalidertSkjema (ValidertSkjema skjema) =
    UvalidertSkjema
        { sertifikatFelt = SertifikatFelt skjema.sertifikatFelt
        , utsteder = skjema.utsteder
        , fullførtMåned = skjema.fullførtMåned
        , fullførtÅr = skjema.fullførtÅr
        , utløperMåned = skjema.utløperMåned
        , utløperÅr = skjema.utløperÅr
        }



-- ENCODE --


encode : ValidertSertifikatSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "label", Json.Encode.string (SertifikatFelt.label skjema.sertifikatFelt) )
      , ( "konseptid", (SertifikatFelt.konseptId >> Json.Encode.int) skjema.sertifikatFelt )
      ]
    ]
        |> List.concat
        |> Json.Encode.object
