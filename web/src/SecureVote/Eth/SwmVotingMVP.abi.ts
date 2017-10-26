export default {
    fullAbi: [{
        "constant": true,
        "inputs": [],
        "name": "nVotesCast",
        "outputs": [{"name": "", "type": "uint256"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "getBallotOptions",
        "outputs": [{"name": "", "type": "uint8[2][4]"}],
        "payable": false,
        "stateMutability": "pure",
        "type": "function"
    }, {
        "constant": false,
        "inputs": [{"name": "encryptedBallot", "type": "bytes32"}, {"name": "senderPubkey", "type": "bytes32"}],
        "name": "submitBallot",
        "outputs": [],
        "payable": false,
        "stateMutability": "nonpayable",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "ballotEncryptionPubkey",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "ballotEncryptionSeckey",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "endTime",
        "outputs": [{"name": "", "type": "uint256"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "getEncSeckey",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "address"}],
        "name": "voterToBallotID",
        "outputs": [{"name": "", "type": "uint256"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "getBallotOptNumber",
        "outputs": [{"name": "", "type": "uint256"}],
        "payable": false,
        "stateMutability": "pure",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "address"}],
        "name": "bannedAddresses",
        "outputs": [{"name": "", "type": "bool"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "uint256"}],
        "name": "associatedAddresses",
        "outputs": [{"name": "", "type": "address"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "swarmFundAddress",
        "outputs": [{"name": "", "type": "address"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "getEncPubkey",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "uint256"}],
        "name": "encryptedBallots",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "startTime",
        "outputs": [{"name": "", "type": "uint256"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": false,
        "inputs": [{"name": "_addr", "type": "address"}],
        "name": "banAddress",
        "outputs": [],
        "payable": false,
        "stateMutability": "nonpayable",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "owner",
        "outputs": [{"name": "", "type": "address"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "uint256"}],
        "name": "associatedPubkeys",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": false,
        "inputs": [{"name": "_secKey", "type": "bytes32"}],
        "name": "revealSeckey",
        "outputs": [],
        "payable": false,
        "stateMutability": "nonpayable",
        "type": "function"
    }, {
        "constant": false,
        "inputs": [{"name": "newEndTime", "type": "uint256"}],
        "name": "setEndTime",
        "outputs": [],
        "payable": false,
        "stateMutability": "nonpayable",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "testMode",
        "outputs": [{"name": "", "type": "bool"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "inputs": [{"name": "_startTime", "type": "uint256"}, {"name": "_endTime", "type": "uint256"}, {
            "name": "_encPK",
            "type": "bytes32"
        }, {"name": "enableTesting", "type": "bool"}],
        "payable": false,
        "stateMutability": "nonpayable",
        "type": "constructor"
    }, {
        "anonymous": false,
        "inputs": [{"indexed": false, "name": "creator", "type": "address"}, {
            "indexed": false,
            "name": "start",
            "type": "uint256"
        }, {"indexed": false, "name": "end", "type": "uint256"}, {
            "indexed": false,
            "name": "encPubkey",
            "type": "bytes32"
        }],
        "name": "CreatedBallot",
        "type": "event"
    }, {
        "anonymous": false,
        "inputs": [{"indexed": false, "name": "voter", "type": "address"}, {
            "indexed": false,
            "name": "reason",
            "type": "string"
        }],
        "name": "FailedVote",
        "type": "event"
    }, {
        "anonymous": false,
        "inputs": [{"indexed": false, "name": "voter", "type": "address"}, {
            "indexed": false,
            "name": "ballot",
            "type": "bytes32"
        }, {"indexed": false, "name": "pubkey", "type": "bytes32"}],
        "name": "SuccessfulVote",
        "type": "event"
    }, {
        "anonymous": false,
        "inputs": [{"indexed": false, "name": "secretKey", "type": "bytes32"}],
        "name": "SeckeyRevealed",
        "type": "event"
    }, {"anonymous": false, "inputs": [], "name": "TestingEnabled", "type": "event"}, {
        "anonymous": false,
        "inputs": [{"indexed": false, "name": "error", "type": "string"}],
        "name": "Error",
        "type": "event"
    }],
    //
    //
    // smaller ABI with stuff stripped out for convenience
    //
    //
    miniAbi: [{
        "constant": true,
        "inputs": [],
        "name": "getBallotOptions",
        "outputs": [{"name": "", "type": "uint8[2][4]"}],
        "payable": false,
        "stateMutability": "pure",
        "type": "function"
    }, {
        "constant": false,
        "inputs": [{"name": "encryptedBallot", "type": "bytes32"}, {"name": "senderPubkey", "type": "bytes32"}],
        "name": "submitBallot",
        "outputs": [],
        "payable": false,
        "stateMutability": "nonpayable",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "address"}],
        "name": "voterToBallotID",
        "outputs": [{"name": "", "type": "uint256"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "getBallotOptNumber",
        "outputs": [{"name": "", "type": "uint256"}],
        "payable": false,
        "stateMutability": "pure",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "uint256"}],
        "name": "associatedAddresses",
        "outputs": [{"name": "", "type": "address"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [],
        "name": "getEncPubkey",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "uint256"}],
        "name": "encryptedBallots",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }, {
        "constant": true,
        "inputs": [{"name": "", "type": "uint256"}],
        "name": "associatedPubkeys",
        "outputs": [{"name": "", "type": "bytes32"}],
        "payable": false,
        "stateMutability": "view",
        "type": "function"
    }]
};
