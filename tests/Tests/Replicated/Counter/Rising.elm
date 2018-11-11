module Tests.Replicated.Counter.Rising exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as List
import Replicated
import Replicated.Counter.Rising as RisingCounter exposing (RisingCounter)
import Replicated.Properties as Properties exposing (Event(..))
import Test exposing (..)


suite : Test
suite =
    describe "Replicated.Rising.Counter"
        [ describe "init"
            [ test "has a zero view" <|
                \_ ->
                    RisingCounter.init "local"
                        |> RisingCounter.view
                        |> Expect.equal 0
            ]
        , describe "incr"
            [ test "increments the view" <|
                \_ ->
                    RisingCounter.init "local"
                        |> RisingCounter.incr
                        |> RisingCounter.view
                        |> Expect.equal 1
            ]
        , fuzz (countersAndEvents 3) "is eventually consistent" <|
            Properties.eventuallyConsistent testConfig
        , fuzz (countersAndEvents 3) "preserves the sum total of ticks" <|
            \( initClocks, events ) ->
                let
                    expectedView =
                        events
                            |> Properties.eventsToOps
                            |> viewForOps
                in
                ( initClocks, events )
                    |> Properties.runAndSyncAll testConfig
                    |> Properties.eachState (RisingCounter.view >> Expect.equal expectedView)
        ]


viewForOps : List ( String, Operation ) -> Int
viewForOps =
    List.map Tuple.second >> List.count ((==) Incr)


countersAndEvents : Int -> Fuzzer ( Dict String RisingCounter, List (Event Operation) )
countersAndEvents =
    Properties.replicasWithEvents RisingCounter.config operation


testConfig =
    Properties.TestConfig RisingCounter.config run


type Operation
    = Incr


operation : Fuzzer Operation
operation =
    Fuzz.constant Incr


run : Operation -> RisingCounter -> RisingCounter
run (Incr) =
    RisingCounter.incr
