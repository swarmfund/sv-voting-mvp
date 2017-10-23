module ChecksumTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SecureVote.Eth.Utils exposing (isValidEthAddress)
import Test exposing (..)


etherSuite : Test
etherSuite =
    describe "Etherum tests"
        [ describe "Address Checksum"
            [ test "test dev example" <|
                \_ ->
                    "0xa74476443119A942dE498590Fe1f2454d7D4aC0d"
                        |> isValidEthAddress
                        |> Expect.equal True
            , test "test another example" <|
                \_ ->
                    "0xc1912fee45d61c87cc5ea59dae31190fffff232d"
                        |> isValidEthAddress
                        |> Expect.equal True
            , test "empty string" <|
                \_ ->
                    ""
                        |> isValidEthAddress
                        |> Expect.equal False
            , test "test without 0x" <|
                \_ ->
                    "c1912fee45d61c87cc5ea59dae31190fffff232d"
                        |> isValidEthAddress
                        |> Expect.equal False
            , test "test all uppercase" <|
                \_ ->
                    "0XC1912FEE45D61C87CC5EA59DAE31190FFFFF232D"
                        |> isValidEthAddress
                        |> Expect.equal False
            , test "test some upper case" <|
                \_ ->
                    "0xc1912fEE45d61C87Cc5EA59DaE31190FFFFf232d"
                        |> isValidEthAddress
                        |> Expect.equal True
            , test "wrong checksum" <|
                \_ ->
                    "0xC1912fEE45d61C87Cc5EA59DaE31190FFFFf232d"
                        |> isValidEthAddress
                        |> Expect.equal False
            , test "test example List" <|
                \_ ->
                    let
                        lhsAnswers =
                            List.map (\_ -> True) exampleList

                        rhsAnswers =
                            List.map isValidEthAddress exampleList
                    in
                    Expect.equalLists lhsAnswers rhsAnswers
            ]
        ]


exampleList : List String
exampleList =
    [ "0x634d31118a7d5f4fbc3ee8b837daf7d246dc9580"
    , "0x3f5ce5fbfe3e9af3971dd833d26ba9b5c936f0be"
    , "0xbcc78b4cbf66a1ce31fbe0e967e578ebcb09c04c"
    , "0x00f78d023dd008c6e8bd3e16327466d58fb6b576"
    , "0xae42f8f37371fd03002edf5635424a51b9c51880"
    , "0x634d31118a7d5f4fbc3ee8b837daf7d246dc9580"
    , "0x6c15291028d082e1b9358e19f15c83b9c54f2ba1"
    , "0x114804f9aa8043769e9a6e006832a1471b28f47f"
    , "0xb3be68d0c3fc49d33ccd6b7aa31c874a9df0c8a4"
    , "0xf65ec42763880b2180926bcc36bdc32a5ea1dc04"
    , "0x324c7e344541ce5993c51a97cfbcd221e964e1c5"
    , "0xbcc78b4cbf66a1ce31fbe0e967e578ebcb09c04c"
    , "0xe6a32c32e83a44cbf0ff41b52d0800973b794051"
    , "0x457368965e247e8a837eba201e40ed5549d92a0c"
    , "0x76618ee5605bf1ebc3c4071541a9745fe8613d71"
    , "0xb66fab39d8743f7c7f053a47e5617528c717f61a"
    , "0x464b0b37db1ee1b5fbe27300acfbf172fd5e4f53"
    , "0x5d061f43b39d625719c6fd77e420d19cc36b6167"
    , "0x25e924fe826f240f3a8e0d2b0c9ca2222dd9a5d4"
    , "0xc1182432112164319cbc38e9261c304b25a73f1c"
    ]
