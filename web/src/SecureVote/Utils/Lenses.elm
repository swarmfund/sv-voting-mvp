module SecureVote.Utils.Lenses exposing (..)

import Dict exposing (Dict)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)


(=|>) : Optional a b -> Lens b c -> Optional a c
(=|>) o l =
    let
        g =
            o.getOption >> Maybe.map l.get

        s : c -> a -> a
        s c a =
            let
                bM =
                    o.getOption a
            in
            case Maybe.map (l.set c) bM of
                Just b ->
                    o.set b a

                Nothing ->
                    a
    in
    Optional g s


dictWDE : comparable -> Optional (Dict comparable (Dict k v)) (Dict k v)
dictWDE key =
    Optional
        (Dict.get key >> Maybe.withDefault Dict.empty >> Just)
        (Dict.insert key)
