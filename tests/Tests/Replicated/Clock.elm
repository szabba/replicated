module Tests.Replicated.Clock exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Replicated
import Replicated.Clock as Clock exposing (Clock)
import Replicated.Properties as Properties exposing (Event(..))
import Test exposing (..)


suite : Test
suite =
    describe "Replicated.Clock"
        [ describe "init"
            [ test "retains the id with a zero count" <|
                \_ ->
                    Clock.init "local"
                        |> Clock.view
                        |> Expect.equal (Dict.fromList [ ( "local", 0 ) ])
            ]
        , describe "tick"
            [ test "increments the count for the local ID" <|
                \_ ->
                    Clock.init "local"
                        |> Clock.tick
                        |> Clock.view
                        |> Dict.get "local"
                        |> Expect.equal (Just 1)
            , test "only increments the local ID's count" <|
                \_ ->
                    let
                        expectedView =
                            Dict.fromList
                                [ ( "local", 1 )
                                , ( "remote", 0 )
                                ]

                        remoteClock =
                            Clock.init "remote"

                        clock =
                            Clock.init "local"
                                |> Clock.merge (remoteClock |> Clock.diff ())
                    in
                    clock
                        |> Clock.tick
                        |> Clock.view
                        |> Expect.equal expectedView
            ]
        , fuzz (clocksAndEvents 3) "is eventually consistent" <|
            Properties.eventuallyConsistent testConfig
        , fuzz (clocksAndEvents 3) "preserves the sum total of ticks" <|
            \( initClocks, events ) ->
                let
                    expectedView =
                        events
                            |> Properties.eventsToOps
                            |> viewForOps initClocks
                in
                ( initClocks, events )
                    |> Properties.runAndSyncAll testConfig
                    |> Properties.eachState (Clock.view >> Expect.equal expectedView)
        ]


viewForOps : Dict String Clock -> List ( String, Operation ) -> Dict String Int
viewForOps initClocks locatedOps =
    let
        initCounts =
            initClocks |> Dict.map (\_ _ -> 0)

        countTick ( nodeID, Tick ) acc =
            acc |> Dict.update nodeID (Maybe.map ((+) 1))
    in
    locatedOps |> List.foldl countTick initCounts


clocksAndEvents : Int -> Fuzzer ( Dict String Clock, List (Event Operation) )
clocksAndEvents =
    Properties.replicasWithEvents Clock.config operation


testConfig =
    Properties.TestConfig Clock.config run


type Operation
    = Tick


operation : Fuzzer Operation
operation =
    Fuzz.constant Tick


run : Operation -> Clock -> Clock
run Tick =
    Clock.tick
