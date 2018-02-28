pragma solidity ^0.4.19;

//
// LittleBallotBox
// Single use contract to manage a ballot (based on SwarmVotingMVP)
// Author: Max Kaye
//
//
// Architecture:
// * Ballot authority declares public key with which to encrypt ballots (optional)
// * Users submit encrypted or plaintext ballots as blobs (dependent on above)
// * These ballots are tracked by the ETH address of the sender
// * Following the conclusion of the ballot, the secret key is provided
//   by the ballot authority, and all users may transparently and
//   independently validate the results
//
// Notes:
// * Since ballots are encrypted the only validation we can do is length, but UI takes care of most of it
//


contract LittleBallotBox {
    //// ** Storage Variables

    // Std owner pattern
    address public owner;

    // test mode - operations like changing start/end times
    bool public testMode = false;

    // Maps to store ballots, along with corresponding log of voters.
    // Should only be modified through `addBallotAndVoter` internal function
    mapping (uint256 => bytes32) public ballotMap;
    mapping (uint256 => bytes32) public associatedPubkeys;
    mapping (uint256 => address) public associatedAddresses;
    uint256 public nVotesCast = 0;

    // Use a map for voters to look up their ballot
    mapping (address => uint256) public voterToBallotID;

    // NOTE - We don't actually want to include the PublicKey because _it's included in the ballotSpec_.
    // It's better to ensure ppl actually have the ballot spec by not including it in the contract.
    // Plus we're already storing the hash of the ballotSpec anyway...

    // Private key to be set after ballot conclusion - curve25519
    bytes32 public ballotEncryptionSeckey;
    bool seckeyRevealed = false;

    // Timestamps for start and end of ballot (UTC)
    uint64 public startTime = 0;
    uint64 public endTime;

    // specHash by which to validate the ballots integrity
    bytes32 public specHash;
    bool public useEncryption;

    // deprecation flag - doesn't actually do anything besides signal that this contract is deprecated;
    bool public deprecated = false;

    //// ** Events
    event CreatedBallot(address creator, uint256 start, uint256 end, bool encrypted, bytes32 specHash);
    event SuccessfulPkVote(address voter, bytes32 ballot, bytes32 pubkey);
    event SuccessfulVote(address voter, bytes32 ballot);
    event SeckeyRevealed(bytes32 secretKey);
    event TestingEnabled();
    event Error(string error);
    event DeprecatedContract();


    //// ** Modifiers

    modifier onlyOwner {
        require(msg.sender == owner);
        _;
    }

    modifier ballotOpen {
        require(block.timestamp >= startTime && block.timestamp < endTime);
        _;
    }

    modifier onlyTesting {
        require(testMode);
        _;
    }

    modifier isTrue(bool _b) {
        require(_b);
        _;
    }

    modifier isFalse(bool _b) {
        require(!_b);
        _;
    }

    //// ** Functions

    // Constructor function - init core params on deploy
    function LittleBallotBox(bytes32 _specHash, uint64 _startTime, uint64 _endTime, bool _useEncryption, bool enableTesting) public {
        owner = msg.sender;

        // take the max of the start time provided and the blocks timestamp to avoid a DoS against recent token holders
        // (which someone might be able to do if they could set the timestamp in the past)
        startTime = max(_startTime, block.timestamp);
        endTime = _endTime;
        useEncryption = _useEncryption;
        specHash = _specHash;

        if (enableTesting) {
            testMode = true;
            TestingEnabled();
        }

        CreatedBallot(msg.sender, _startTime, _endTime, _useEncryption, _specHash);
    }

    // Ballot submission
    function submitBallotWithPk(bytes32 encryptedBallot, bytes32 senderPubkey) isTrue(useEncryption) ballotOpen public {
        addBallotAndVoter(encryptedBallot, senderPubkey);
        SuccessfulPkVote(msg.sender, encryptedBallot, senderPubkey);
    }

    function submitBallotNoPK(bytes32 ballot) isFalse(useEncryption) ballotOpen public {
        addBallotAndVoterNoPk(ballot);
        SuccessfulVote(msg.sender, ballot);
    }

    // Internal function to ensure atomicity of voter log
    function addBallotAndVoter(bytes32 encryptedBallot, bytes32 senderPubkey) internal {
        uint256 ballotNumber = addBallotAndVoterNoPk(encryptedBallot);
        associatedPubkeys[ballotNumber] = senderPubkey;
    }

    function addBallotAndVoterNoPk(bytes32 encryptedBallot) internal returns (uint256) {
        uint256 ballotNumber = nVotesCast;
        ballotMap[ballotNumber] = encryptedBallot;
        associatedAddresses[ballotNumber] = msg.sender;
        voterToBallotID[msg.sender] = ballotNumber;
        nVotesCast += 1;
        return ballotNumber;
    }

    // Allow the owner to reveal the secret key after ballot conclusion
    function revealSeckey(bytes32 _secKey) onlyOwner public {
        require(block.timestamp > endTime);

        ballotEncryptionSeckey = _secKey;
        seckeyRevealed = true; // this flag allows the contract to be locked
        SeckeyRevealed(_secKey);
    }

    function getEncSeckey() public constant returns (bytes32) {
        return ballotEncryptionSeckey;
    }

    // Test functions
    function setEndTime(uint64 newEndTime) onlyTesting onlyOwner public {
        endTime = newEndTime;
    }

    function setDeprecated() onlyOwner public {
        deprecated = true;
        DeprecatedContract();
    }
}
