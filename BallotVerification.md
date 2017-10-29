# Ballot Verification

This process will allow you to verify your ballot was constructed and cast as intended.

From the voting site, you should have details like these:

```javascript
var ballotEncPk = "14108a8f844f506d91daedfd3542425bd5b4e2b1b3e7d959a2344907689dea7e";
var myPubkey = "3687cd75e4415e4b126583751b66c8a3b17eaf8eaaa0754a4638452d5602ce4c";
var mySeckey = "e314013176e70a9cc4b46c96b93115ade6ec66843cdba1fb23544345683c9b9b";
var myDelegate = "0x9999999999999999999999999999999999999999";
var myVotesRaw = [1,3,-3,0];
var myVotesOffset = [4,6,0,3];
var encBallot = "0325252d3e6bd1b7e5bf085096bb07e3132d114623e123d9ebd48d411fb65ad2";
var submitBallotPrefix = "13c04769";
var txData = "0x13c047690325252d3e6bd1b7e5bf085096bb07e3132d114623e123d9ebd48d411fb65ad23687cd75e4415e4b126583751b66c8a3b17eaf8eaaa0754a4638452d5602ce4c";
var votingContract = "0x2Bb10945E9f0C9483022dc473aB4951BC2a77d0f";
```

## Scaffold of process (todo)

1. Raw votes are processed by adding 3 (takes from range `[-3,3] -> [0,6]`)
2. Change raw votes to bit strings of len 3 (e.g. `0 -> "000", 3 -> "011", 6 -> "110"`)
3. Concatenate these bit strings in order (e.g. `[4,6,0,3] -> "100110000011"`)
4. Pad _right_ with 4x `0`s (e.g. `"100110000011" -> "1001100000110000"`)
5. Convert the bit string to 2x bytes (e.g. "1001100000110000" -> [0x98, 0x30])
6. Take the left-most 14 bytes of the delegate address (all 9s in this case)
7. Concatenate to form a 16 byte string - call this `ballotPlaintext`
8. Calculate the encryption nonce (`var nonce = sha256(fromHex(myPubkey)).slice(0,24)`)
9. Feed this into the encryption module `nacl.crypto_box(ballotPlaintext, nonce, ballotEncPk, mySeckey)`
10. The result should match `encBallot`
11. Full result is valid if `txData == "0x" + submitBallotPrefix + encBallot + myPubkey`
12. You can also double check that `txData` matches the data in your transaction broadcast to the Ethereum network.

