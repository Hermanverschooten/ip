module Subnet exposing
    ( validate
    , fromCIDR, toCIDR
    , base, hosts, included
    )

{-| This library contains a number of functions for working with subnets.
It can validate subnet mask strings, see if an address is inside a given subnet.

#Validation

@docs validate

#CIDR

@docs fromCIDR, toCIDR

#Common functionality

@docs base, hosts, included

-}

import Utils exposing (..)



--- INTERNAL ---


toBits : Int -> Int
toBits b =
    8 - truncate (logBase 2 (toFloat (256 - b)))


toInt : Maybe String -> Int
toInt maybe_ip =
    case maybe_ip of
        Just ip ->
            case parts ip of
                [ Just n1, Just n2, Just n3, Just n4 ] ->
                    (n1 * 256 ^ 3) + (n2 * 256 ^ 2) + (n3 * 256) + n4

                _ ->
                    0

        Nothing ->
            0


toString : List Int -> Maybe String
toString p =
    if List.length p == 4 then
        Just
            (List.map (\i -> String.fromInt i) p
                |> String.join "."
            )

    else
        Nothing



--- PUBLIC ---


{-| Check to see if the given subnet is valid.

    validate "255.255.255.0" == True

    validate "255.0.255.128" == False

-}
validate : String -> Bool
validate subnet =
    let
        isPower : Int -> Bool
        isPower i =
            let
                h =
                    logBase 2 (toFloat (256 - i))
            in
            (h == toFloat (truncate h)) && h <= 8
    in
    case parts subnet of
        [ Just n1, Just n2, Just n3, Just n4 ] ->
            case [ n4, n3, n2, n1 ] of
                [ n, 255, 255, 255 ] ->
                    n >= 0 && isPower n

                [ 0, n, 255, 255 ] ->
                    n >= 0 && isPower n

                [ 0, 0, n, 255 ] ->
                    n >= 0 && isPower n

                [ 0, 0, 0, n ] ->
                    n >= 0 && isPower n

                _ ->
                    False

        _ ->
            False


{-| Calculates the CIDR length in bits from a subnet mask.

    toCIDR "255.255.255.0" == 24

    toCIDR "255.255.254.0" == 23

-}
toCIDR : String -> Int
toCIDR subnet =
    case parts subnet of
        [ Just n1, Just n2, Just n3, Just n4 ] ->
            case [ n1, n2, n3, n4 ] of
                [ 255, 255, 255, n ] ->
                    24 + toBits n

                [ 255, 255, n, 0 ] ->
                    16 + toBits n

                [ 255, n, 0, 0 ] ->
                    8 + toBits n

                [ n, 0, 0, 0 ] ->
                    toBits n

                _ ->
                    0

        _ ->
            0


{-| Returns the base IP of the given subnet.

    base ("192.168.1.25", "255.255.255.0") == Just "192.168.1.0"

    base ("192.168.1.25:, "255.255.255.252") == Just "192.168.1.24"

    base ("invalid ip", "or invalid subnet") == Nothing

-}
base : ( String, String ) -> Maybe String
base ( ip, subnet ) =
    let
        calc : Int -> Int -> Int
        calc sub_ ip_ =
            let
                bits_ =
                    case truncate (logBase 2 (toFloat (256 - sub_))) of
                        0 ->
                            0

                        b ->
                            2 ^ b
            in
            (ip_ // bits_) * bits_
    in
    case ( parts ip, parts subnet ) of
        ( [ Just i1, Just i2, Just i3, Just i4 ], [ Just n1, Just n2, Just n3, Just n4 ] ) ->
            case [ n1, n2, n3, n4 ] of
                [ 255, 255, 255, n ] ->
                    toString [ i1, i2, i3, calc n i4 ]

                [ 255, 255, n, 0 ] ->
                    toString [ i1, i2, calc n i3, 0 ]

                [ 255, n, 0, 0 ] ->
                    toString [ i1, calc n i2, 0, 0 ]

                [ n, 0, 0, 0 ] ->
                    toString [ calc n i1, 0, 0, 0 ]

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Get the String representation for a given CIDR.

    fromCIDR 24 == Just "255.255.255.0"

-}
fromCIDR : Int -> Maybe String
fromCIDR cidr =
    let
        calc : Int -> Int
        calc cidr_ =
            256 - (2 ^ (8 - modBy 8 cidr))
    in
    toString
        (case cidr // 8 of
            3 ->
                [ 255, 255, 255, calc cidr ]

            2 ->
                [ 255, 255, calc cidr, 0 ]

            1 ->
                [ 255, calc cidr, 0, 0 ]

            0 ->
                [ calc cidr, 0, 0, 0 ]

            _ ->
                []
        )


{-| Give a subnet mask, calculates the number of host possible.

    hosts "255.255.255.0" == 256

-}
hosts : String -> Int
hosts subnet =
    case toCIDR subnet of
        0 ->
            0

        n ->
            2 ^ (32 - n)


{-| Is the IP address in the subnet.

    included ( "192.168.1.0", "255.255.255.0" ) "192.168.1.5" == True

    included ( "192.168.17.0", "255.255.254.0" ) "192.168.18.5" == False

-}
included : ( String, String ) -> String -> Bool
included ( lower, subnet ) ip =
    let
        base_ =
            toInt (base ( lower, subnet ))

        end_ =
            base_ + hosts subnet

        ip_ =
            toInt (Just ip)
    in
    (base_ <= ip_) && (end_ >= ip_)
