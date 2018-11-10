-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Replicated.Properties exposing
    ( Event(..)
    , TestConfig
    , eachState
    , eventsToOps
    , eventuallyConsistent
    , replicasWithEvents
    , runAndSyncAll
    , syncTo
    )

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List.Extra as List
import Replicated exposing (Config)


type alias TestConfig typ version update view operation =
    { config : Config typ version update view
    , run : operation -> typ -> typ
    }


type Event operation
    = Operation String operation
    | Sync String String


syncTo : Config typ version update view -> view -> ( typ, typ ) -> Expectation
syncTo ({ view } as config) expectedView clocks =
    clocks
        |> Replicated.sync config
        |> Expect.all
            [ Tuple.first >> view >> Expect.equal expectedView
            , Tuple.second >> view >> Expect.equal expectedView
            ]


replicasWithEvents :
    { a | init : String -> typ }
    -> Fuzzer operation
    -> Int
    -> Fuzzer ( Dict String typ, List (Event operation) )
replicasWithEvents { init } operation count =
    if count <= 0 then
        Fuzz.constant ( Dict.empty, [] )

    else
        let
            initNth n =
                ( String.fromInt n
                , init <| String.fromInt n
                )

            states =
                initNth
                    |> List.initialize count
                    |> Dict.fromList
        in
        count
            |> randomEvent operation
            |> Fuzz.list
            |> Fuzz.map (Tuple.pair states)


randomEvent : Fuzzer operation -> Int -> Fuzzer (Event operation)
randomEvent operation replicas =
    Fuzz.frequency
        [ ( 10, Fuzz.map2 Operation (id replicas) operation )
        , ( 1, Fuzz.map2 Sync (id replicas) (id replicas) )
        ]


id : Int -> Fuzzer String
id count =
    Fuzz.intRange 0 (count - 1)
        |> Fuzz.map String.fromInt


eventsToOps : List (Event operation) -> List ( String, operation )
eventsToOps =
    List.filterMap <|
        \evt ->
            case evt of
                Operation nodeID op ->
                    Just ( nodeID, op )

                _ ->
                    Nothing


runAndSyncAll :
    TestConfig typ version update view operation
    -> ( Dict String typ, List (Event operation) )
    -> Dict String typ
runAndSyncAll testConfig ( initStates, events ) =
    let
        nodeIDs =
            initStates |> Dict.keys

        allSyncs =
            nodeIDs |> selfCrossMap Sync
    in
    (events ++ allSyncs)
        |> List.foldl (processEvent testConfig) initStates


eachState : (typ -> Expectation) -> Dict String typ -> Expectation
eachState expect states =
    let
        expectForNode nodeID =
            Dict.get nodeID >> Maybe.map expect >> Maybe.withDefault Expect.pass

        expectations =
            states
                |> Dict.keys
                |> List.map expectForNode
    in
    states |> Expect.all expectations


eventuallyConsistent :
    TestConfig typ version update view operation
    -> ( Dict String typ, List (Event operation) )
    -> Expectation
eventuallyConsistent ({ config } as testConfig) =
    runAndSyncAll testConfig >> allViewsEqual config


processEvent :
    TestConfig typ version update view operation
    -> Event operation
    -> Dict String typ
    -> Dict String typ
processEvent { config, run } event states =
    case event of
        Operation on op ->
            states
                |> Dict.update on (Maybe.map (run op))

        Sync leftID rightID ->
            let
                reinsert ( left, right ) =
                    states
                        |> Dict.insert leftID left
                        |> Dict.insert rightID right
            in
            Maybe.map2 Tuple.pair
                (states |> Dict.get leftID)
                (states |> Dict.get rightID)
                |> Maybe.map (Replicated.sync config >> reinsert)
                |> Maybe.withDefault states


allViewsEqual : { a | view : typ -> view } -> Dict String typ -> Expectation
allViewsEqual config states =
    states
        |> Expect.all (states |> Dict.keys |> selfCrossMap (equalViews config))


equalViews :
    { a | view : typ -> view }
    -> String
    -> String
    -> Dict String typ
    -> Expectation
equalViews { view } leftID rightID states =
    let
        failMsg =
            "cannot compare " ++ leftID ++ " and " ++ rightID ++ " nodes - at least one is missing"

        leftView =
            states |> Dict.get leftID |> Maybe.map view

        rightView =
            states |> Dict.get rightID |> Maybe.map view
    in
    Maybe.map2 Expect.equal leftView rightView
        |> Maybe.withDefault (Expect.fail failMsg)


selfCrossMap : (a -> a -> b) -> List a -> List b
selfCrossMap f l =
    l |> List.concatMap (\el -> List.map (f el) l)
