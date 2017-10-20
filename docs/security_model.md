# Security Model for Swarm Voting MVP

This document outlines the reasoning behind our choice in crypto primitives and the conclusions we can draw from their use. 
It also outlines the process for voting, how that can remain secure, and how it enables small encrypted ballots with (somewhat) deterministic encryption.

## Overview

Each voter submits an encrypted ballot.
An encrypted ballot is used to prevent the knowledge of existing votes from altering the outcome of the ballot.
The public key for this encryption is a curve25519 key, made available before the ballot takes place (32 bytes).
After the conclusion of the ballot the secret key will be made available, allowing any auditor to validate the result of the ballot.

Any Ethereum account may cast a ballot, HOWEVER, the audit software will look up each account's SWM token balance and use this to weight the result.
Those addresses with 0 SWM balance (most addresses out there) will have a vote weight of 0, meaning outside participation in the ballot (by people without SWM tokens) is irrelevant.

We (ab)use a particular method of encryption via curve25519 that guarantees (somewhat) deterministic encryption.
This is usually undesirable, however, in this case it enables voters to fully validate their ballot before and after casting.

## Deterministic-ish Curve25519 Encryption

Curve25519 allows for asymmetric authenticated encryption.
This requires the sender to use a Curve25519 keypair, as well as the receiver.
Anonymous encryption makes use of an ephemeral key on the part of the sender and 0s the relevant memory to ensure the keypair used for encryption can never be recovered.
As our voters do not have Curve21559 keys we use an ephemeral key to encrypt the ballot.

To produce deterministic encryption with Curve25519 we take the following measures:

* Use the popular `sodium` library
* Use the typically non-deterministic `crypto_box` suite
* Generate a secret key used to encrypt the message
* Set the curve25519 nonce to the first 24 bytes of the SHA256 hash of the ephemeral public key
* Provide both the generated secret key, nonce, and ballot plaintext to the voter
* The voter can then deterministically reproduce the resultant ciphertext

Usually, at least the nonce is chosen randomly, and for anonymous encryption the secret key is also chosen randomly.

By using a deterministic nonce and generating an ephemeral secret key (provided to the voter), we can produce deterministic (and short!) ciphertext. 
Under some circumstances a deterministic nonce would be undesirable, but because the nonce is public anyway, the output of sha256(publicKey) is psuedorandom, and this key is only used once, we can do this safely.

## Byte Layout

```
+-----------------------------+
|                             |
|  Ballot: 12 + 4 bits        |
|                             |
+-----------------------------+
|                             |
|  Delegate address: 112 bits |
|      (prefix) - 14 bytes    |
|                             |
+-----------------------------+
``` 

The ballots themselves require only 12 bits. We pad this (on the right) with 0s to give us 2 bytes (16 bits).

We then use the remaining space for the leading 14 bytes of the delegate address.

We then encrypt the plaintext, which gives us pseudorandom ciphertext.

To recover, we simply decrypt the encrypted ballot.

We can then count the ballots as per usual, optionally delegating where a delegate has a) voted or delegated, and b) where the delegate is not the 0x0 address.

## Getting to 64 byte encrypted ballots

For constants see [this documentation](https://github.com/paixaop/node-sodium/blob/master/docs/low-level-api.md#constants-1).

The first 16 bytes of ciphertext from `crypto_box` are all 0s. We can truncate them. Some implementations do this automatically.
This is given by `crypto_box_BOXZEROBYTES`.

The length of the output of `crypto_box` is:

* `crypto_box_ZEROBYTES` + `length of message (16 bytes)`
* = 32 + 16 = 48

But we can truncate the first 16 bytes, getting us back to 32.

We additionally need the ephemeral public key (32 bytes), though we don't need the nonce as this is generated from the public key (it would normally be another 24 bytes).

The total is thus 64 bytes.
