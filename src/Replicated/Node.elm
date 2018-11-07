-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Replicated.Node exposing (ID, Node, id, newNode, root)


type Node
    = Node { id_ : ID }


type ID
    = ID String


root : Node
root =
    Node { id_ = ID "" }


newNode : Node -> { created : Node, updated : Node }
newNode creator =
    { created = creator
    , updated = creator
    }


id : Node -> ID
id (Node { id_ }) =
    id_
