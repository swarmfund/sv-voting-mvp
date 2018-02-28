pragma solidity ^0.4.19;


import { LittleBallotBox } from "./LittleBallotBox.sol";


contract LittleGovIndex {

    address public owner;

    struct Ballot {
        bytes32 specHash;
        bytes32 extraData;
        address votingContract;
    }

    struct Democ {
        string name;
        address admin;
        Ballot[] ballots;
    }

    mapping (bytes32 => Democ) democs;
    bytes32[] public democList;

    // addresses that do not have to pay for democs
    mapping (address => bool) public democWhitelist;
    // democs that do not have to pay for issues
    mapping (address => bool) public issueWhitelist;

    // payment details
    address public payTo;
    uint256 public requiredEthForDemoc;
    uint256 public requiredEthForIssue;
    bool public paymentEnabled = false;

    //* EVENTS /

    event PaymentMade(uint256 value, uint256 remainder, address sender, address paidTo);
    event NoPayment(address sender);
    event DemocInit(string name, bytes32 democHash, address admin);

    //* MODIFIERS /

    modifier onlyBy(address _account) {
        require(msg.sender == _account);
        _;
    }

    modifier payReq(mapping (address => bool) whitelist, uint256 v) {
        // check whitelists - do not require payment in some cases
        if (paymentEnabled && !whitelist[msg.sender]) {
            require(msg.value >= v);

            // handle payments
            uint256 remainder = msg.value - v;
            payTo.transfer(v); // .transfer so it throws on failure
            msg.sender.transfer(remainder);
            PaymentMade(v, remainder, msg.sender, payTo);
        } else {
            NoPayment(msg.sender);
        }

        // do main
        _;
    }


    //* FUNCTIONS /


    // constructor
    function LittleGovIndex() public {
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

    function setEth(uint256 _newEthPerDemoc, uint256 _newEthPerIssue) onlyBy(owner) public {
        requiredEthForDemoc = _newEthPerDemoc;
        requiredEthForIssue = _newEthPerIssue;
    }

    function setOwner(address _owner) onlyBy(owner) public {
        owner = _owner;
    }

    function setPaymentEnabled(bool _enabled) onlyBy(owner) public {
        paymentEnabled = _enabled;
    }

    function setWhitelistDemoc(address addr, bool _free) onlyBy(owner) public {
        democWhitelist[addr] = _free;
    }

    function setWhitelistIssue(address addr, bool _free) onlyBy(owner) public {
        issueWhitelist[addr] = _free;
    }

    //* DEMOCRACY FUNCTIONS - INDIVIDUAL */

    function initDemoc(string democName) payReq(democWhitelist, requiredEthForDemoc) public payable returns (bytes32) {
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

    function getNthBallot(bytes32 democHash, uint256 n) public constant returns (bytes32 specHash, bytes32 extraData, address votingContract) {
        return (democs[democHash].ballots[n].specHash, democs[democHash].ballots[n].extraData, democs[democHash].ballots[n].votingContract);
    }

    //* ADD BALLOT TO RECORD */

    function _commitBallot(bytes32 democHash, bytes32 specHash, bytes32 extraData, address votingContract) internal {
        democs[democHash].ballots.push(Ballot(specHash, extraData, votingContract));
    }

    function addBallot(bytes32 democHash, bytes32 extraData, address votingContract)
                      onlyBy(democs[democHash].admin)
                      payReq(issueWhitelist, requiredEthForIssue)
                      public
                      payable
                      {
        LittleBallotBox bb = LittleBallotBox(votingContract);
        bytes32 specHash = bb.specHash();
        _commitBallot(democHash, specHash, extraData, votingContract);
    }

    function deployBallot(bytes32 democHash, bytes32 specHash, bytes32 extraData,
                          uint64 startTime, uint64 endTime, bool useEncryption, bool testing)
                          onlyBy(democs[democHash].admin)
                          payReq(issueWhitelist, requiredEthForIssue)
                          public payable {
        // the start time is max(startTime, block.timestamp) to avoid a DoS whereby a malicious electioneer could disenfranchise
        // token holders who have recently acquired tokens.
        LittleBallotBox votingContract = new LittleBallotBox(specHash, max(startTime, block.timestamp), endTime, useEncryption, testing);
        _commitBallot(democHash, specHash, extraData, address(votingContract));
    }
}
