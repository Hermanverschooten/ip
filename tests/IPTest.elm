module IPTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import IP
import Test exposing (..)


suite : Test
suite =
    describe "Validating an IP address"
        [ test "with valid IP" <|
            \_ -> Expect.equal True (IP.validate "192.168.1.1")
        , test "with invalid IP" <|
            \_ -> Expect.equal False (IP.validate "192..168.1.1")
        ]
