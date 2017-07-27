module TestPuppetry exposing (..)

import Test exposing (..)
import Puppetry exposing (..)

suite: Test
suite =
    describe "The puppetry module"
        [ test "strip contruction" <|
            \() ->
                let
                    s = strip 3 "Teststrip"
                in
                    Expect.equal "Teststrip" s.name
        ]
