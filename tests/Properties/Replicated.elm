module Properties.Replicated exposing (Config, syncTo)

import Expect exposing (Expectation)
import Replicated exposing (Config)


syncTo : Config typ version update view -> view -> ( typ, typ ) -> Expectation
syncTo ({ view } as config) expectedView clocks =
    clocks
        |> Replicated.sync config
        |> Expect.all
            [ Tuple.first >> view >> Expect.equal expectedView
            , Tuple.second >> view >> Expect.equal expectedView
            ]
