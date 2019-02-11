module SubnetTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Subnet
import Test exposing (..)


suite : Test
suite =
    describe "Subnet tests"
        [ describe "Validating a subnet mask"
            [ test "valid" <|
                \_ -> Expect.equal True (Subnet.validate "255.255.252.0")
            , test "invalid" <|
                \_ -> Expect.equal False (Subnet.validate "255.128.255.0")
            ]
        , describe "toCIDR"
            [ test "valid" <|
                \_ -> Expect.equal 24 (Subnet.toCIDR "255.255.255.0")
            , test "invalid" <|
                \_ -> Expect.equal 0 (Subnet.toCIDR "255.255..0")
            ]
        , describe "base"
            [ test "192.168.1.10/24" <|
                \_ -> Expect.equal (Just "192.168.1.0") (Subnet.base ( "192.168.1.10", "255.255.255.0" ))
            , test "192.168.17.10/23" <|
                \_ -> Expect.equal (Just "192.168.16.0") (Subnet.base ( "192.168.17.10", "255.255.254.0" ))
            ]
        , describe "fromCIDR"
            [ test "/24" <|
                \_ -> Expect.equal (Just "255.255.255.0") (Subnet.fromCIDR 24)
            , test "/23" <|
                \_ -> Expect.equal (Just "255.255.254.0") (Subnet.fromCIDR 23)
            , test "/8" <|
                \_ -> Expect.equal (Just "255.0.0.0") (Subnet.fromCIDR 8)
            ]
        , describe "hosts"
            [ test "/24" <|
                \_ -> Expect.equal 256 (Subnet.hosts "255.255.255.0")
            , test "/23" <|
                \_ -> Expect.equal 512 (Subnet.hosts "255.255.254.0")
            ]
        , describe "included"
            [ test "included in range" <|
                \_ -> Expect.equal True (Subnet.included ( "192.168.1.0", "255.255.255.0" ) "192.168.1.10")
            , test "not included in range" <|
                \_ -> Expect.equal False (Subnet.included ( "192.168.1.0", "255.255.255.0" ) "192.168.10.10")
            ]
        ]
