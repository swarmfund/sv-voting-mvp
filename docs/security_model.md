# Security Model for Swarm Voting MVP

This document outlines the reasoning behind our choice in crypto primitives and the conclusions we can draw from their use. 
It also outlines the process for voting, how that can remain secure, and how it enables small encrypted ballots with (somewhat) deterministic encryption.

## Overview

Each voter submits an encrypted ballot.
An encrypted ballot is used to prevent existing votes from altering the outcome of the ballot.
The public key for this encryption is a curve25519 key, made available before the ballot takes place (32 bytes).
After the conclusion of the ballot the secret key will be made available, allowing any auditor to validate the result of the ballot.

Any Ethereum account may cast a ballot, HOWEVER, the audit software will look up each account's SWM token balance and use this to weight the result.
Those addresses with 0 SWM balance (most addresses out there) will have a vote weight of 0, meaning outside participation in the ballot (by people without SWM tokens) is irrelevant.

We (ab)use a particular method of encryption via curve25519 that guarantees (somewhat) deterministic encryption.
This is usually undesirable, however, in this case it enables voters to fully validate their ballot before and after casting.
Normally, when a user encrypts plaintext using an asymmetric scheme it is desirable for the ciphertext to be non-deterministic (and the definition of perfect secrecy requires this).
Since we are interested in a deterministic process (such that it is able to be validated by a prudent voter), we do not want this feature.

## Deterministic Curve25519 Encryption

Curve25519 allows for asymmetric authenticated encryption.
This requires the sender to use a Curve25519 keypair, as well as the receiver.
Anonymous encryption makes use of an ephemeral key on the part of the sender and 0s the relevant memory to ensure the keypair used for encryption can never be recovered.
As our voters do not have Curve21559 keys (and it would increase the size of the ballot if they did) we use an ephemeral key to encrypt the ballot.

To produce deterministic encryption with Curve25519 we take the following measures:

* Use the popular `sodium` library
* Use the typically non-deterministic `crypto_box` suite
* Generate a secret key used to encrypt the message
* Set the nonce to the first 24 bytes of the SHA3 hash of the ephemeral public key

Usually, at least the nonce is chosen randomly, and for anonymous encryption the secret key is also chosen randomly.

By using a deterministic nonce and generating an ephemeral secret key (provided to the voter), we can produce deterministic (and short!) ciphertext. 
Under some circumstances, this would be undesirable, but for verification purposes it can be used securely.
Since this ephemeral voting key is only used once, using a nonce in this way is okay - the nonce must be public in any case for a given ciphertext.

## Maintining Secrecy

It's worth noting that this alone doesn't give us the properties we want. For example:

```
> s.api.crypto_box(Buffer.from("testmsg"), nonce, pk, sk)
<Buffer 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 31 08 ee 0b 54 be f2 6f 51 d4 bb dd 19 8f f4 61 f0 db 4d 41 ff 8f f8>
> s.api.crypto_box(Buffer.from("testmsf"), nonce, pk, sk)
<Buffer 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 db 08 45 2d 30 ce 95 a7 fe d6 97 32 9a 92 cc da f0 db 4d 41 ff 8f f9>
```

Observe the last 7 bytes and the messages:

* `testmsg` and `f0 db 4d 41 ff 8f f8`
* `testmsf` and `f0 db 4d 41 ff 8f f9`

This similarity is not desirable.

#### Solution

The solution is to encrypt a bytestring with high entropy. We do this by separating the plaintext ballot into three sections (totalling 16 bytes or 128 bits):

```
+-----------------------------+
|                             |
|  Nonce: all remaining bits  |
|  (e.g. 36 in this example)  |
|                             |
+-----------------------------+
|                             |
|  Ballot: Up to 12 bits      |
|                             |
+-----------------------------+
|                             |
|  Delegate address: 80 bits  |
|      (prefix)               |
|                             |
+-----------------------------+
``` 

Note: **this** nonce is generated randomly and provided to the user.

We then take the SHA3 of the ballot nonce (padded with 0s), truncate to the first 80+12 bits, and XOR this with the ballot to generate the high-entropy-plaintext.

We then encrypt the high-entropy-plaintext, which gives us pseudorandom ciphertext.

To recover, we first decrypt the encrypted ballot, take the first 36 bits, take the SHA3, truncate, and XOR with the high-entropy-plaintext to recover the ballot plaintext.
We can then count the ballots as per usual, optionally delegating where a delegate has a) voted or delegated, and b) where the delegate is not the 0x0 address.

## Getting to 64 byte encrypted ballots

For constants see [this documentation](https://github.com/paixaop/node-sodium/blob/master/docs/low-level-api.md#constants-1).

The first 16 bytes of ciphertext from `crypto_box` are all 0s. We can truncate them.
This is given by `crypto_box_BOXZEROBYTES`.

The length of the output of `crypto_box` is:

* `crypto_box_ZEROBYTES` + `length of message (16 bytes)`
* = 32 + 16 = 48

But we can truncate the first 16 bytes, getting us back to 32.

We additionally need the ephemeral public key (32 bytes), though we don't need the nonce as this is generated from the public key (it would normally be another 24 bytes).

The total is thus 64 bytes.
