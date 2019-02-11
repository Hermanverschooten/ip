module IP exposing (validate)

{-| This library allows you to validate a IP address.

#Validation

@docs validate

-}

import Regex
import Utils exposing (..)



--- PUBLIC ---


{-| Check to see if the given IP Address is valid.

    validate "192.168.0.1" == True

    validate "320.16.1.1" == False

-}
validate : String -> Bool
validate ip =
    case parts ip of
        [ Just n1, Just n2, Just n3, Just n4 ] ->
            (n1 > 0 && n1 < 256)
                && (n2 >= 0 && n2 < 256)
                && (n3 >= 0 && n3 < 256)
                && (n4 > 0 && n4 < 256)

        _ ->
            False
