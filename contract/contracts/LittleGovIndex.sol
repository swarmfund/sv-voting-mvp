pragma solidity ^0.4.19;


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

    // Events
    event PaymentMade(uint256 value, uint256 remainder, address sender, address paidTo);
    event NoPayment(address sender);
    event DemocInit(string name, bytes32 democHash, address admin);

    // modifiers
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
            payTo.transfer(v);  // .transfer so it throws on failure
            msg.sender.transfer(remainder);
            PaymentMade(v, remainder, msg.sender, payTo);
        } else {
            NoPayment(msg.sender);
        }

        // do main
        _;
    }


    // functions

    // constructor - v simple
    function LittleGovIndex() public {
        owner = msg.sender;
        payTo = msg.sender;
    }

    // payment & owner functions
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

    // democracy wide functions
    function initDemoc(string democName) payReq(democWhitelist, requiredEthForDemoc) public payable returns (bytes32) {
        bytes32 democHash = keccak256(democName, msg.sender, democList.length);
        democList.push(democHash);
        democs[democHash].name = democName;
        democs[democHash].admin = msg.sender;
        DemocInit(democName, democHash, msg.sender);
        return democHash;
    }

    function setAdmin(bytes32 democHash, address newAdmin) onlyBy(democs[democHash].admin) public {
        democs[democHash].admin = newAdmin;
    }

    // issue functions
    function addBallot(bytes32 democHash,
                       bytes32 ballotSpecHash,
                       bytes32 extraData,
                       address votingContract
                      )
                      onlyBy(democs[democHash].admin)
                      payReq(issueWhitelist, requiredEthForIssue)
                      public
                      payable
                      {
        democs[democHash].ballots.push(Ballot(ballotSpecHash, extraData, votingContract));
    }
}
