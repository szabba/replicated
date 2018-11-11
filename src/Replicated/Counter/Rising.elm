-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Replicated.Counter.Rising exposing (RisingCounter, config, incr, init, view)

import Dict exposing (Dict)
import Replicated exposing (Config)
import Replicated.Clock as Clock exposing (Clock)


type RisingCounter
    = RisingCounter Guts


type alias Guts =
    { id : String
    , clock : Clock
    }


config : Config RisingCounter Clock (Dict String Int) Int
config =
    { init = init, view = view, version = version, diff = diff, merge = merge }


init : String -> RisingCounter
init nodeID =
    RisingCounter
        { id = nodeID
        , clock = Clock.init nodeID
        }


incr : RisingCounter -> RisingCounter
incr (RisingCounter ({ clock } as guts)) =
    RisingCounter { guts | clock = Clock.tick clock }


view : RisingCounter -> Int
view (RisingCounter { clock }) =
    clock
        |> Clock.view
        |> Dict.values
        |> List.sum


version : RisingCounter -> Clock
version (RisingCounter { clock }) =
    clock


diff : Clock -> RisingCounter -> Dict String Int
diff _ (RisingCounter { clock }) =
    Clock.diff () clock


merge : Dict String Int -> RisingCounter -> RisingCounter
merge deltas (RisingCounter ({ clock } as guts)) =
    let
        newClock =
            clock |> Clock.merge deltas
    in
    RisingCounter { guts | clock = newClock}
