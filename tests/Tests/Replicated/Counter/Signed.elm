module Tests.Replicated.Counter.Signed exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as List
import Replicated
import Replicated.Counter.Signed as SignedCounter exposing (SignedCounter)
import Replicated.Properties as Properties exposing (Event(..))
import Test exposing (..)


suite : Test
suite =
    describe "Replicated.Counter.Signed"
        [ describe "init"
            [ test "has a zero view" <|
                \_ ->
                    SignedCounter.init "local"
                        |> SignedCounter.view
                        |> Expect.equal 0
            ]
        , describe "incr"
            [ test "increments the view" <|
                \_ ->
                    SignedCounter.init "local"
                        |> SignedCounter.incr
                        |> SignedCounter.view
                        |> Expect.equal 1
            ]
        , describe "decr"
            [ test "decrements the view" <|
                \_ ->
                    SignedCounter.init "local"
                        |> SignedCounter.decr
                        |> SignedCounter.view
                        |> Expect.equal -1
            ]
        , fuzz (countersAndEvents 3) "is eventually consistent" <|
            Properties.eventuallyConsistent testConfig
        , fuzz (countersAndEvents 3) "preserves the sum total" <|
            \( initClocks, events ) ->
                let
                    expectedView =
                        events
                            |> Properties.eventsToOps
                            |> viewForOps
                in
                ( initClocks, events )
                    |> Properties.runAndSyncAll testConfig
                    |> Properties.eachState (SignedCounter.view >> Expect.equal expectedView)
        ]


viewForOps : List ( String, Operation ) -> Int
viewForOps =
    List.map Tuple.second
        >> List.map operationDelta
        >> List.sum


operationDelta : Operation -> Int
operationDelta op =
    case op of
        Incr ->
            1

        Decr ->
            -1


countersAndEvents : Int -> Fuzzer ( Dict String SignedCounter, List (Event Operation) )
countersAndEvents =
    Properties.replicasWithEvents SignedCounter.config operation


testConfig =
    Properties.TestConfig SignedCounter.config run


type Operation
    = Incr
    | Decr


operation : Fuzzer Operation
operation =
    Fuzz.oneOf [ Fuzz.constant Incr, Fuzz.constant Decr ]


run : Operation -> SignedCounter -> SignedCounter
run op =
    case op of
        Incr ->
            SignedCounter.incr

        Decr ->
            SignedCounter.decr
