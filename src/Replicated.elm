module Replicated exposing (Config, sync)


type alias Config typ version update view =
    { init : String -> typ
    , view : typ -> view
    , version : typ -> version
    , diff : version -> typ -> update
    , merge : update -> typ -> typ
    }


sync :
    Config typ version update view
    -> ( typ, typ )
    -> ( typ, typ )
sync { diff, version, merge } ( left, right ) =
    let
        leftDiff =
            diff (version left) left

        rightDiff =
            diff (version right) right
    in
    ( left |> merge rightDiff
    , right |> merge leftDiff
    )
