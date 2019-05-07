module Utils exposing (..)

chunksOf3 : [a] -> [[a]]
chunksOf3 xs =
    case xs of
        [] -> []
        x :: y :: z :: rest ->
            [x,y,z] :: chunksOf3 rest
        rest ->
            rest
