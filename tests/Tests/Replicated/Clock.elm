module Tests.Replicated.Clock exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as List
import Properties.Replicated as Properties
import Random
import Replicated
import Replicated.Clock as Clock exposing (Clock, config)
import Shrink
import Test exposing (..)


suite : Test
suite =
    describe "Replicated.Clock"
        [ describe "init"
            [ fuzz string "retains the id with a zero count" <|
                \id ->
                    config.init id
                        |> config.view
                        |> Expect.equal (Dict.fromList [ ( id, 0 ) ])
            ]
        , describe "tick"
            [ fuzz2 string (Fuzz.intRange 1 10) "increments the count for the local ID" <|
                \id times ->
                    let
                        clock =
                            config.init id
                    in
                    clock
                        |> repeat times Clock.tick
                        |> config.view
                        |> Dict.get (Clock.localID clock)
                        |> Expect.equal (Just times)
            , fuzz (Fuzz.intRange 0 10) "only increments the local ID's count" <|
                \times ->
                    let
                        expectedView =
                            Dict.fromList
                                [ ( "local", times )
                                , ( "remote", 0 )
                                ]

                        remoteClock =
                            config.init "remote"

                        clock =
                            config.init "local"
                                |> config.merge (remoteClock |> config.diff ())
                    in
                    clock
                        |> repeat times Clock.tick
                        |> config.view
                        |> Expect.equal expectedView
            ]
        , describe "merge"
            [ fuzz2 (Fuzz.intRange 0 10) (Fuzz.intRange 0 10) "merges two previously independent clocks" <|
                \localTicks remoteTicks ->
                    let
                        expectedView =
                            Dict.fromList [ ( "local", localTicks ), ( "remote", remoteTicks ) ]

                        localClock =
                            Clock.init "local" |> repeat localTicks Clock.tick

                        remoteClock =
                            Clock.init "remote" |> repeat remoteTicks Clock.tick
                    in
                    ( localClock, remoteClock )
                        |> Properties.syncTo Clock.config expectedView
            , test "merges two independent clocks that have synced before" <|
                \_ ->
                    let
                        expectedView =
                            Dict.fromList [ ( "local", 3 ), ( "remote", 5 ) ]

                        clocks =
                            ( Clock.init "local" |> repeat 1 Clock.tick
                            , Clock.init "remote" |> repeat 2 Clock.tick
                            )

                        updatedClocks =
                            Replicated.sync Clock.config clocks
                                |> Tuple.mapFirst (repeat 2 Clock.tick)
                                |> Tuple.mapSecond (repeat 3 Clock.tick)
                    in
                    updatedClocks
                        |> Properties.syncTo Clock.config expectedView
            ]
        ]



-- TODO: test syncs interleaved with updates, followed by a full sync results in everyone having the same view


repeat : Int -> (a -> a) -> a -> a
repeat n f x =
    if n > 0 then
        repeat (n - 1) f (f x)

    else
        x


syncTo : Dict String Int -> ( Clock, Clock ) -> Expectation
syncTo expectedView clocks =
    clocks
        |> Replicated.sync Clock.config
        |> Expect.all
            [ Tuple.first >> Clock.view >> Expect.equal expectedView
            , Tuple.second >> Clock.view >> Expect.equal expectedView
            ]
