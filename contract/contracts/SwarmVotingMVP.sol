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
    bytes32[2][] public encryptedBallots;
    address[] public voterLog;
    uint256 public nVotes = 0;

    // Public key with which to encrypt ballots - curve25519
    bytes32 public ballotEncryptionPubkey;

    // Private key to be set after ballot conclusion - curve25519
    bytes32 public ballotEncryptionSeckey;
    bool seckeyRevealed = false;

    // Timestamps for start and end of ballot (UTC)
    uint256 public startTime;
    uint256 public endTime;

    // Banned addresses - necessary to ban Swarm Fund from voting in their own ballot
    mapping(address => bool) public bannedAddresses;
    address public swarmFundAddress = 0x0;


    //// ** Events
    event CreatedBallot(address creator, uint256 start, uint256 end, bytes32 encPubkey);
    event FailedVote(address voter, string reason);
    event SuccessfulVote(address voter, bytes32[2] ballot);
    event SeckeyRevealed(bytes32 secretKey);


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
        require(block.timestamp > startTime);
        require(block.timestamp < endTime);
        _;
    }

    //// ** Functions

    // Constructor function - init core params on deploy
    function SwarmVotingMVP(uint256 _startTime, uint256 _endTime, bytes32 _encPK) public {
        owner = msg.sender;

        startTime = _startTime;
        endTime = _endTime;
        ballotEncryptionPubkey = _encPK;

        bannedAddresses[swarmFundAddress] = true;
    }

    // Ballot submission
    function submitBallot(bytes32[2] encryptedBallot) notBanned ballotOpen public {
        addBallotAndVoter(encryptedBallot);
        SuccessfulVote(msg.sender, encryptedBallot);
    }

    // Internal function to ensure atomicity of voter log
    function addBallotAndVoter(bytes32[2] encryptedBallot) internal {
        encryptedBallots.push(encryptedBallot);
        voterLog.push(msg.sender);
    }

    // Allow the owner to reveal the secret key after ballot conclusion
    function revealSeckey(bytes32 _secKey) onlyOwner public {
        require(block.timestamp > endTime);

        ballotEncryptionSeckey = _secKey;
        seckeyRevealed = true;  // this flag allows the contract to be locked
        SeckeyRevealed(_secKey);
    }

    // Finalize ballot, allow no further interactions
    // TODO : Do we want to lock the ballot? Any reason?
//    function finalizeBallot() onlyOwner public {
//        require(block.timestamp > endTime);
//        require(seckeyRevealed);
//    }

    // Helpers
    function getEncPubkey() public constant returns (bytes32) {
        return ballotEncryptionPubkey;
    }

    function getEncSeckey() public constant returns (bytes32) {
        return ballotEncryptionSeckey;
    }
}
