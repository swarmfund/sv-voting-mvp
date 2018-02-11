module VBitTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List as List
import SecureVote.Types.VBit exposing (VBit(OneBit, ZeroBit), vBitsToBytes)
import Test exposing (..)


vBits1 =
    [ ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, OneBit ]


vBits255 =
    [ OneBit, OneBit, OneBit, OneBit, OneBit, OneBit, OneBit, OneBit ]


vBits128 =
    [ OneBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit ]


vBits16 =
    [ ZeroBit, ZeroBit, ZeroBit, OneBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit ]


suite : Test
suite =
    describe "SecureVote MVP + Supporting code for Swarm's Liquid Range Vote"
        [ describe "vBitsToBytes"
            [ test "correctly converts VBits to bytes" <|
                \_ ->
                    let
                        pairs =
                            [ ( [], [] )
                            , ( [ 0 ], [ ZeroBit ] )
                            , ( [ 1 ], vBits1 )
                            , ( [ 2 ], [ ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, OneBit, ZeroBit ] )
                            , ( [ 4 ], [ ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, OneBit, ZeroBit, ZeroBit ] )
                            , ( [ 8 ], [ ZeroBit, ZeroBit, ZeroBit, ZeroBit, OneBit, ZeroBit, ZeroBit, ZeroBit ] )
                            , ( [ 16 ], [ ZeroBit, ZeroBit, ZeroBit, OneBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit ] )
                            , ( [ 128 ], [ OneBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit, ZeroBit ] )
                            , ( [ 255 ], vBits255 )

                            -- we pad right
                            , ( [ 128 ], [ OneBit ] )
                            , ( [ 192 ], [ OneBit, OneBit ] )
                            , ( [ 1, 1 ], vBits1 ++ vBits1 )
                            , ( [ 255, 255 ], vBits255 ++ vBits255 )
                            , ( [ 255, 128 ], vBits255 ++ vBits128 )
                            , ( [ 255, 128 ], vBits255 ++ [ OneBit ] )
                            , ( [ 16, 96 ], vBits16 ++ [ ZeroBit, OneBit, OneBit, ZeroBit ] )
                            ]

                        lhsAnswers =
                            List.map Tuple.first pairs

                        rhsAnswers =
                            List.map (\( lhs, rhs ) -> vBitsToBytes (ceiling <| toFloat (List.length rhs) / 8) rhs) pairs
                    in
                    Expect.equalLists lhsAnswers rhsAnswers
            ]
        ]
