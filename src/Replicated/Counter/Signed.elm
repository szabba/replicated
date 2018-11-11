-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Replicated.Counter.Signed exposing (SignedCounter, config, decr, diff, incr, init, merge, view)

import Dict exposing (Dict)
import Replicated exposing (Config)
import Replicated.Clock as Clock exposing (Clock)


type SignedCounter
    = SignedCounter Guts


type alias Guts =
    { id : String
    , perNode : Dict String Counts
    }


type alias Counts =
    { incrs : Int
    , decrs : Int
    }


config : Config SignedCounter () (Dict String ( Int, Int )) Int
config =
    { init = init, view = view, version = always (), diff = diff, merge = merge }


init : String -> SignedCounter
init id =
    SignedCounter <| Guts id <| Dict.fromList [ ( id, Counts 0 0 ) ]


view : SignedCounter -> Int
view (SignedCounter { perNode }) =
    perNode
        |> Dict.values
        |> List.map (\counts -> counts.incrs - counts.decrs)
        |> List.sum


incr : SignedCounter -> SignedCounter
incr =
    updateLocal <| \counts -> { counts | incrs = counts.incrs + 1 }


decr : SignedCounter -> SignedCounter
decr =
    updateLocal <| \counts -> { counts | decrs = counts.decrs + 1 }


updateLocal : (Counts -> Counts) -> SignedCounter -> SignedCounter
updateLocal f (SignedCounter ({ id, perNode } as guts)) =
    let
        newCounts =
            perNode |> Dict.update id (Maybe.map f)
    in
    SignedCounter { guts | perNode = newCounts }


diff : () -> SignedCounter -> Dict String ( Int, Int )
diff () (SignedCounter { perNode }) =
    perNode
        |> Dict.map (\_ { incrs, decrs } -> ( incrs, decrs ))


merge : Dict String ( Int, Int ) -> SignedCounter -> SignedCounter
merge update (SignedCounter ({ perNode } as guts)) =
    let
        insertRemote k ( incrs, decrs ) =
            Dict.insert k <| Counts incrs decrs

        mergeTwo k ( remoteIncrs, remoteDecrs ) { incrs, decrs } =
            Dict.insert k <| Counts (max remoteIncrs incrs) (max remoteDecrs decrs)

        newPerNode =
            Dict.empty
                |> Dict.merge insertRemote mergeTwo Dict.insert update perNode
    in
    SignedCounter { guts | perNode = newPerNode }
