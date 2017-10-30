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

## Deterministically validating your ballot

The above information has everything you need to verify your ballot was constructed honestly.

We're going to go through a number of steps that transform your ballot from what you intend it to be, into the data structure recorded in the blockchain.

To follow along, you'll need to have `node js` installed, and install a few libraries:

* `mkdir -p ballotVerification && cd ballotVerification`
* `npm install js-nacl ramda`

Next, open a node REPL by running `node` and import `js-nacl`. Copy in and run these commands:

```javascript
var jsNacl = require('js-nacl')
var R = require('ramda')
var nacl; jsNacl.instantiate(_nacl => { nacl = _nacl; })
```

Finally, paste in the variables you received from the SWM Release Schedule voting site.

Now you're ready to go.

### 1. Offset your raw votes

Each vote you cast is in the range `[-3, 3]`, but that's a bit annoying to store (worrying about signs and all that) so instead we translate each vote to the range `[0, 6]` by adding `3` to each vote.

This is done for you in the above list of variables: `myVotesRaw` are the original votes, and `myVotesOffset` are the translated votes.

### 2. Convert the offset votes to binary

Next, we need to convert your _offset_ votes to binary. We can use the following table for this:

* `0 -> "000"`
* `1 -> "001"`
* `2 -> "010"`
* `3 -> "011"`
* `4 -> "100"`
* `5 -> "101"`
* `6 -> "110"`

Run in node:

```javascript
binaryVotes = R.map(v => {
    var b = v.toString(2);
    return R.join('', R.repeat('0', 3 - b.length)) + b;
}, myVotesOffset)
```

### 3. Concatenate your votes

We need to move all your votes into one string. The way we do this is by concatenating them in order.

If your votes were `[1, 3, -3, 0]` (and therefore `` when offset) then your binary representation would be `['100', '110', '000', '011']`.

We then concatenate these into one string: `'100110000011'`

Run in node:

```javascript
binVotesUnpadded = R.join('', binaryVotes)
```

### 4. Pad right with `0`s

We're going to take this string of 12 bits and transform it into 2 bytes, for that we need 16 bits total.

Add 4 `0`s to the right of your string: `'1001100000110000'`

```javascript
binVotes = binVotesUnpadded + R.join('', R.repeat('0', 16 - binVotesUnpadded.length))
```

### 5. Convert to bytes

We need to convert this bit-string to bytes.

Following our example:

* `'1001100000110000'` becomes `['10011000', '00110000']`
* `['10011000', '00110000']` becomes `[0x98, 0x30]` (which is `[152, 48]` in decimal)

```javascript
voteBytes = R.map(bStr => parseInt(bStr, 2), R.splitAt(8, binVotes))
```

### 6. Take the left-most 14 bytes of your delegate address

If you haven't selected a delegate then your delegate address is set to `0x9999999999999999999999999999999999999999`. This is okay, since nobody can ever own the account `0x9999999999999999999999999999999999999999` (or, if they did then Ethereum's entire security model would be broken). This works the same for any arbitrarily (or randomly) chosen address, like `0x0000000000000000000000000000000000000000`, or `0x1111111111111111111111111111111111111111`.

In our example, `'0x9999999999999999999999999999999999999999'` becomes `'9999999999999999999999999999'`


```javascript
delegatePrefix = R.take(14*2, R.drop(2, myDelegate))
```

### 7. Calculate the ballot plaintext

We have everything we need to create your unencrypted ballot now.

In our example we woudl end up with `'98309999999999999999999999999999'`

```javascript
ballotPlaintext = nacl.to_hex(voteBytes) + delegatePrefix
```

### 8. Calculate your encryption nonce

Using Curve25519 through the `NaCl` library requires using a nonce during encryption. To avoid including an extra 24 bytes with your ballot unnecessarily, we use something we can generate from data we already have: your public key. Since we're only going to use the curve25519 key pair we generated _just_ for one ballot, this is okay, since we're going to throw away the key pair after we're done voting.

```javascript
nonce = nacl.to_hex(nacl.crypto_hash_sha256(nacl.from_hex(myPubkey)).slice(0, 24))
```

This will generate a `Uint8Array` so won't look like hex anymore.

### 9. Encrypt your ballot

Now we need to encrypt your ballot:

```javascript
generatedEncBallot = nacl.to_hex(nacl.crypto_box.apply(null, R.map(nacl.from_hex, [ballotPlaintext, nonce, ballotEncPk, mySeckey])))
``` 

This will give you a 32 byte encrypted ballot.

You can verify this matches what the voting UI generated with:

```javascript
encBallot == generatedEncBallot 
```

(this should equal `true`)

### 10. Generate transaction data

Next, we'll confirm the transaction data we broadcast to Ethereum was accurate:

```javascript
generatedTxData = "0x" + submitBallotPrefix + generatedEncBallot + myPubkey
```

Again, we can verify this matches our transaction with:

```javascript
txData == generatedTxData
```

Which should once again equal `true`

### 11. Double check this matches what you broadcast to the ethereum network

At this point you should look up your transaction via `etherscan.io` or your preferred Ethereum block explorer.

Copy your input data into node to confirm:

```javascript
var dataInEthereum = "YOUR TX DATA HERE"
dataInEthereum == generatedTxData
```

If this returns `true` then you can be 100% sure your ballot was generated and recorded honestly and accurately.

