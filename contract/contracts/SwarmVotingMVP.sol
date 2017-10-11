pragma solidity ^0.4.17;

//
// Swarm Voting MVP
// Single use contract to manage liquidity vote shortly after Swarm TS
// Author: Max Kaye
//
//
// Architecture:
// * Ballot authority declares public key with which to encrypt ballots
// * Users submit encrypted ballots as blobs
// * These ballots are tracked by the ETH address of the sender
// * Following the conclusion of the ballot, the secret key is provided
//   by the ballot authority, and all users may transparently and
//   independently validate the results
//
// Notes:
// * Since ballots are encrypted the only validation we can do is length
//


contract SwarmVotingMVP {
    //// ** Storage Variables

    // Std owner pattern
    address public owner;

    // Arrays to store ballots, along with corresponding log of voters.
    // Using a list here instead of a mapping so we can easily count the
    // number of votes so we can scan through them later.
    // Should only be modified through `addBallotAndVoter` internal function
    bytes32[] public encryptedBallots;
    address[] public voterLog;

    // Public key with which to encrypt ballots - curve25519 or TBD
    byte[64] public ballotEncryptionPubkey;

    // Private key to be set after ballot conclusion - curve25519 or TBD
    byte[32] public ballotEncryptionSeckey;
    bool seckeyRevealed = false;

    // Timestamps for start and end of ballot (UTC)
    uint256 public startTime;
    uint256 public endTime;

    // Banned addresses - necessary to ban Swarm Fund from voting in their own ballot
    mapping(address => bool) public bannedAddresses;
    address public swarmFundAddress = 0x0;


    //// ** Events
    event CreatedBallot(address creator, uint256 start, uint256 end, byte[64] encPubkey);
    event FailedVote(address voter, string reason);
    event SuccessfulVote(address voter, bytes32 ballot);
    event SeckeyRevealed(byte[32] secretKey);


    //// ** Modifiers

    modifier notBanned {
        require(!bannedAddresses[msg.sender]);  // ensure banned addresses cannot vote
        _;
    }

    modifier onlyOwner {
        require(msg.sender == owner);  // fail if msg.sender is not the owner
        _;
    }

    modifier ballotOpen {
        require(now > startTime);
        require(now < endTime);
        _;
    }

    //// ** Functions

    // Constructor function - init core params on deploy
    function SwarmVotingMVP(uint256 _startTime, uint256 _endTime, byte[64] _encPK) {
        owner = msg.sender;

        startTime = _startTime;
        endTime = _endTime;
        ballotEncryptionPubkey = _encPK;

        bannedAddresses[swarmFundAddress] = true;
    }

    // Ballot submission
    function submitBallot(bytes32 encryptedBallot) notBanned ballotOpen public {
        addBallotAndVoter(encryptedBallot);
        SuccessfulVote(msg.sender, encryptedBallot);
    }

    // Internal function to ensure atomicity of voter log
    function addBallotAndVoter(bytes32 encryptedBallot) internal {
        encryptedBallots.push(encryptedBallot);
        voterLog.push(msg.sender);
    }

    // Allow the owner to reveal the secret key after ballot conclusion
    function revealSeckey(byte[32] _secKey) onlyOwner public {
        require(now > endTime);

        ballotEncryptionSeckey = _secKey;
        seckeyRevealed = true;  // this flag allows the contract to be locked
        SeckeyRevealed(_secKey);
    }

    // Finalize ballot, allow no further interactions
    // TODO : Do we want to lock the ballot? Any reason?
//    function finalizeBallot() onlyOwner public {
//        require(now > endTime);
//        require(seckeyRevealed);
//    }
}
