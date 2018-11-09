-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Replicated.Clock exposing (Clock, config, diff, init, localID, merge, tick, version, view)

import Dict exposing (Dict)
import Replicated exposing (Config)


type Clock
    = Clock String (Dict String Int)


config : Config Clock () (Dict String Int) (Dict String Int)
config =
    { init = init, view = view, version = version, diff = diff, merge = merge }


init : String -> Clock
init id =
    Clock id <| Dict.fromList [ ( id, 0 ) ]


localID : Clock -> String
localID (Clock id _) =
    id


tick : Clock -> Clock
tick (Clock id state) =
    state
        |> Dict.update id (Maybe.map <| (+) 1)
        |> Clock id


view : Clock -> Dict String Int
view (Clock _ state) =
    state


version : Clock -> ()
version _ =
    ()


diff : () -> Clock -> Dict String Int
diff _ (Clock _ state) =
    state


merge : Dict String Int -> Clock -> Clock
merge update (Clock id state) =
    let
        pickMax k l r d =
            Dict.insert k (max l r) d
    in
    Dict.empty
        |> Dict.merge Dict.insert pickMax Dict.insert state update
        |> Clock id
