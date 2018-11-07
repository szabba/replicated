-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Tests.Replicated.Node exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as List
import Random
import Replicated.Node as Node exposing (ID, Node)
import Shrink
import Test exposing (..)


suite : Test
suite =
    describe "Replicated.Node"
        [ describe "newNode"
            [ fuzz (numbersInRisingRange 10) "never generates a node with the root ID" <|
                \nodeNos ->
                    nodeNos
                        |> createNodes
                        |> List.map Node.id
                        |> List.filter ((==) <| Node.id Node.root)
                        |> Expect.equal [ Node.id Node.root ]
            ]
        ]


createNodes : List Int -> List Node
createNodes =
    let
        loop nodes nodeNos =
            case nodeNos of
                [] ->
                    nodes

                no :: leftNos ->
                    let
                        addCreated { created, updated } =
                            List.append
                                (nodes |> List.setAt no updated)
                                [ created ]

                        newNodes =
                            nodes
                                |> List.getAt no
                                |> Maybe.map (Node.newNode >> addCreated)
                                |> Maybe.withDefault nodes
                    in
                    loop newNodes leftNos
    in
    loop [ Node.root ] >> List.reverse >> List.drop 1


numbersInRisingRange : Int -> Fuzzer (List Int)
numbersInRisingRange n =
    let
        generator =
            List.initialize n identity
                |> List.map (Random.int 0)
                |> List.foldr (Random.map2 (::)) (Random.constant [])

        shrinker =
            Shrink.list Shrink.noShrink
                |> Shrink.keepIf (List.indexedMap (\ix c -> c <= ix) >> List.all identity)
                |> Shrink.dropIf List.isEmpty
    in
    Fuzz.custom generator shrinker
