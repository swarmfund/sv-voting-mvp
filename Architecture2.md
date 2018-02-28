# Architecture v2

### Index Contract

1x SC acting as index. Ballots submitted with `addBallot` - most code kept on votingContract themselves. issuesHash is keccak256 of `BallotSpecOuter` JSON doc w/ ballot spec including issues. Ballot spec forward compatible via version tags.

```
addBallot(bytes32 democHash, bytes32 ballotSpecHash, bytes32 extraData, address votingContract)
```

Permissions controlled by whomever calls `initDemoc` - they can submit issues to issues via `addBallot`

```
initDemoc(string democName) public returns (bytes32)

Fires an event: NewDemoc(string democName, address democAdmin, bytes32 democHash)
```

No additional data for ballots in index contract.

Supports payment for democs/ballots via known address, not necessarily used. can be set by `owner`.

### Ballot Contracts

They can be arbitrary but best to keep a canonical set of versions.

Supports encrypted and plaintext ballots (note: not much to do on SC side here)

Supports versioned ballots via version tag - should match ballotSpec version

Note: in the spec below a `Maybe a` is either `null` or an `a`

##### Minimum Supported Interfaces for Ballots

```
uint256 public startTime;  // can be 0 for no startTime
uint256 public endTime;
bytes32 public specHash;  // should be equal to ballotSpecHash in Index
deprecated() public constant returns (bool);  // used to mark ballots as broken if something bad happens
```

### `BallotSpecOuter`

```
{ ballotVersion :: Int
, ballotInner :: BallotSpecVersioned
}
```

### `BallotSpecVersioned`

Polymorphic over `ballotType` (i.e. different internal structures depending on version)

#### `BallotSpecVersioned` (`ballotType = 0x01`)

```
{ ballotTitle :: String
, shortDesc :: String
, longDesc :: String
, startTime :: Int
, endTime :: Int
, erc20Addr :: String
, discussionLink :: Maybe String
, binding :: Bool
, encryptionPK :: Maybe String
, options :: OptionsOuter
}
```

Note: If `encryptionPK` is not null then the ballot SC must support publishing a secret key with method `getEncSecretKey() public constant returns (bytes32)`.

Also note: in an earlier draft startTimes were optional, but this is somewhat unsafe. Better is to go by the _latest_ time out of:

* when the contract was published
* and what the startTime says

So it shouldn't be possible to set the start time so early that current token holders are disenfranchised.

The rule is: if the voting contract has the latest start time, use that. If the start time is set to prior to the contract being published, use the timestamp of when the smart contract was published.

### `OptionsOuter`

```
{ optionsVersion :: Int
, options :: Maybe (Array Option)
}
```

#### `Option` (`optionsVersion == 0x01`) - Range Voting

```
{ optionTitle :: String
, optionDesc :: Maybe String
}
```

#### `Option` (`optionsVersion == 0x02`) - Binary Yes/No

```
null
```
