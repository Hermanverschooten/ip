module Utils exposing (dot, parts)

import Regex


dot : Regex.Regex
dot =
    Maybe.withDefault Regex.never <| Regex.fromString "\\."


parts : String -> List (Maybe Int)
parts p =
    Regex.split dot p |> List.map String.toInt
