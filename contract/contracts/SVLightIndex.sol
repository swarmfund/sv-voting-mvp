pragma solidity ^0.4.19;


//
// The Index by which democracies and ballots are tracked (and optionally deployed).
// Author: Max Kaye <max@secure.vote>
// License: MIT
//


import { SVLightBallotBox } from "./SVLightBallotBox.sol";


contract SVLightIndex {

    address public owner;

    struct Ballot {
        bytes32 specHash;
        bytes32 extraData;
        address votingContract;
        uint64 startTs;
    }

    struct Democ {
        string name;
        address admin;
        Ballot[] ballots;
    }

    mapping (bytes32 => Democ) public democs;
    bytes32[] public democList;

    // addresses that do not have to pay for democs
    mapping (address => bool) public democWhitelist;
    // democs that do not have to pay for issues
    mapping (address => bool) public ballotWhitelist;

    // payment details
    address public payTo;
    // uint128's used because they account for amounts up to 3.4e38 wei or 3.4e20 ether
    uint128 public democFee = 0.05 ether; // 0.05 ether; about $50 at 3 March 2018
    mapping (address => uint128) democFeeFor;
    uint128 public ballotFee = 0.01 ether; // 0.01 ether; about $10 at 3 March 2018
    mapping (address => uint128) ballotFeeFor;
    bool public paymentEnabled = true;

    uint8 constant PAY_DEMOC = 0;
    uint8 constant PAY_BALLOT = 1;

    function getPaymentParams(uint8 paymentType) internal constant returns (bool, uint128, uint128) {
        if (paymentType == PAY_DEMOC) {
            return (democWhitelist[msg.sender], democFee, democFeeFor[msg.sender]);
        } else if (paymentType == PAY_BALLOT) {
            return (ballotWhitelist[msg.sender], ballotFee, ballotFeeFor[msg.sender]);
        } else {
            assert(false);
        }
    }

    //* EVENTS /

    event PaymentMade(uint128[2] valAndRemainder);
    event DemocInit(string name, bytes32 democHash, address admin);
    event BallotInit(bytes32 specHash, uint64[2] openPeriod, bool[2] flags);
    event BallotAdded(bytes32 democHash, bytes32 specHash, bytes32 extraData, address votingContract);
    event SetFees(uint128[2] _newFees);
    event PaymentEnabled(bool _feeEnabled);

    //* MODIFIERS /

    modifier onlyBy(address _account) {
        require(msg.sender == _account);
        _;
    }

    modifier payReq(uint8 paymentType) {
        // get our whitelist, generalFee, and fee's for particular addresses
        bool wl;
        uint128 genFee;
        uint128 feeFor;
        (wl, genFee, feeFor) = getPaymentParams(paymentType);
        // init v to something large in case of exploit or something
        uint128 v = 1000 ether;
        // check whitelists - do not require payment in some cases
        if (paymentEnabled && !wl) {
            v = feeFor;
            if (v == 0){
                // if there's no fee for the individual user then set it to the general fee
                v = genFee;
            }
            require(msg.value >= v);

            // handle payments
            uint128 remainder = uint128(msg.value) - v;
            payTo.transfer(v); // .transfer so it throws on failure
            if (!msg.sender.send(remainder)){
                payTo.transfer(remainder);
            }
            PaymentMade([v, remainder]);
        }

        // do main
        _;
    }


    //* FUNCTIONS /


    // constructor
    function SVLightIndex() public {
        owner = msg.sender;
        payTo = msg.sender;
    }

    //* GLOBAL INFO */

    function nDemocs() public constant returns (uint256) {
        return democList.length;
    }

    //* PAYMENT AND OWNER FUNCTIONS */

    function setPayTo(address newPayTo) onlyBy(owner) public {
        payTo = newPayTo;
    }

    function setEth(uint128[2] newFees) onlyBy(owner) public {
        democFee = newFees[PAY_DEMOC];
        ballotFee = newFees[PAY_BALLOT];
        SetFees([democFee, ballotFee]);
    }

    function setOwner(address _owner) onlyBy(owner) public {
        owner = _owner;
    }

    function setPaymentEnabled(bool _enabled) onlyBy(owner) public {
        paymentEnabled = _enabled;
        PaymentEnabled(_enabled);
    }

    function setWhitelistDemoc(address addr, bool _free) onlyBy(owner) public {
        democWhitelist[addr] = _free;
    }

    function setWhitelistBallot(address addr, bool _free) onlyBy(owner) public {
        ballotWhitelist[addr] = _free;
    }

    function setFeeFor(address addr, uint128[2] fees) onlyBy(owner) public {
        democFeeFor[addr] = fees[PAY_DEMOC];
        ballotFeeFor[addr] = fees[PAY_BALLOT];
    }

    //* DEMOCRACY FUNCTIONS - INDIVIDUAL */

    function initDemoc(string democName) payReq(PAY_DEMOC) public payable returns (bytes32) {
        bytes32 democHash = keccak256(democName, msg.sender, democList.length, this);
        democList.push(democHash);
        democs[democHash].name = democName;
        democs[democHash].admin = msg.sender;
        DemocInit(democName, democHash, msg.sender);
        return democHash;
    }

    function getDemocInfo(bytes32 democHash) public constant returns (string name, address admin, uint256 nBallots) {
        return (democs[democHash].name, democs[democHash].admin, democs[democHash].ballots.length);
    }

    function setAdmin(bytes32 democHash, address newAdmin) onlyBy(democs[democHash].admin) public {
        democs[democHash].admin = newAdmin;
    }

    function nBallots(bytes32 democHash) public constant returns (uint256) {
        return democs[democHash].ballots.length;
    }

    function getNthBallot(bytes32 democHash, uint256 n) public constant returns (bytes32 specHash, bytes32 extraData, address votingContract, uint64 startTime) {
        return (democs[democHash].ballots[n].specHash, democs[democHash].ballots[n].extraData, democs[democHash].ballots[n].votingContract, democs[democHash].ballots[n].startTs);
    }

    //* ADD BALLOT TO RECORD */

    function _commitBallot(bytes32 democHash, bytes32 specHash, bytes32 extraData, address votingContract, uint64 startTs) internal {
        democs[democHash].ballots.push(Ballot(specHash, extraData, votingContract, startTs));
        BallotAdded(democHash, specHash, extraData, votingContract);
    }

    function addBallot(bytes32 democHash, bytes32 extraData, address votingContract)
                      onlyBy(democs[democHash].admin)
                      payReq(PAY_BALLOT)
                      public
                      payable
                      {
        SVLightBallotBox bb = SVLightBallotBox(votingContract);
        bytes32 specHash = bb.specHash();
        uint64 startTs = bb.startTime();
        _commitBallot(democHash, specHash, extraData, votingContract, startTs);
    }

    function deployBallot(bytes32 democHash, bytes32 specHash, bytes32 extraData,
                          uint64[2] openPeriod, bool[2] flags)
                          onlyBy(democs[democHash].admin)
                          payReq(PAY_BALLOT)
                          public payable {
        // the start time is max(startTime, block.timestamp) to avoid a DoS whereby a malicious electioneer could disenfranchise
        // token holders who have recently acquired tokens.
        uint64 startTs = max(openPeriod[0], uint64(block.timestamp));
        SVLightBallotBox votingContract = new SVLightBallotBox(specHash, [startTs, openPeriod[1]], flags);
        votingContract.setOwner(msg.sender);
        _commitBallot(democHash, specHash, extraData, address(votingContract), startTs);
        BallotInit(specHash, [startTs, openPeriod[1]], flags);
    }

    // utils
    function max(uint64 a, uint64 b) pure internal returns(uint64) {
        if (a > b) {
            return a;
        }
        return b;
    }
}
