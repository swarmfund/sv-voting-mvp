var SwarmVotingMVP = artifacts.require("./SwarmVotingMVP.sol");
var VotingCF = SwarmVotingMVP;  // voting contract factory

var naclJs = require('js-nacl');
var crypto = require('crypto');

const {create, env} = require('sanctuary');
const S = create({checkTypes: true, env});


const bytes32zero = "0x0000000000000000000000000000000000000000000000000000000000000000";

// todo: confirm
const swmFund = "0x8bf7b2d536d286b9c5ad9d99f608e9e214de63f0";


const genRandomBytes32 = () => {
  return "0x" + crypto.randomBytes(32).toString('hex');
}


let accounts;


contract('SwarmVotingMVP', function(_accounts) {
  accounts = _accounts;

  var hexSk = "0xcd9d715f05a4fce8acf3339fd5ee8549c1899c52e4b32da07cffcd91a29ad976";
  var hexPk = "0xba781ed1006bd7694282a210485265f1c503f4e6721858b4269ae6d745f7bb4b";

  // note: these need to match vars in `2_deploy_contracts.js`
  var startTime = Math.round(Date.now() / 1000) + 1;
  var endTime = startTime + 600;
  var shortEndTime = 0;

  it("should instantiate correctly", async function () {
    try {
      let vc = await VotingCF.deployed();
    
      const owner = await vc.owner();
      assert.equal(owner, accounts[0], "Owner must be set on launch.");

      const _earlyVote = await vc.submitBallot(hexPk, hexPk, {from: accounts[111]});
      assertOnlyEvent('Error', _earlyVote);

      const snapshot = await getSnapshot();

      await timeTravel(11);
      
      const _startTime = await vc.startTime();
      assert.isAtLeast(startTime + 2, _startTime, "Start time matches. (2s window above)");
      assert.isAtMost(startTime - 2, _startTime, "Start time matches. (2s window below)");
      
      const _endTime = await vc.endTime();
      assert.isAtLeast(endTime + 2, _endTime, "endTime in 2s window (above)");
      assert.isAtMost(endTime - 2, _endTime, "endTime in 2s window (below)");

      await timeTravel(1);
      
      const _testMode = await vc.testMode();
      assert.equal(_testMode, true, "We should be in test mode");
      
      const _nVotes = await vc.nVotesCast();
      assert.equal(_nVotes, 0, "Should have no votes at start");
      
      const _pk = await vc.ballotEncryptionPubkey();
      assert.equal(_pk, hexPk, "hex pk should match");

      const _sk = await vc.ballotEncryptionSeckey();
      assert.equal(_sk, bytes32zero, "ballot enc key should be zeros before reveal")

      const _swmBanned = await vc.bannedAddresses(swmFund);
      assert.equal(_swmBanned, true, "swm fund should be banned from ballot");

      const _ballotOptions = await vc.getBallotOptions();
      const _ballotOptionsProcd = S.map(([a, b]) => [a.toNumber(), b.toNumber()], _ballotOptions);
      assert.deepEqual(_ballotOptionsProcd, [[8, 42], [42, 8], [16, 42], [1, 42]], "should have expected ballot params")

      const _ballotOptNumber = await vc.getBallotOptNumber();
      assert.equal(_ballotOptNumber, 4, "should have 4 ballot options")

      //// ASSERTIONS FOR INSTANTIATION COMPLETE
      await testABallot(S.Just(vc), S.Just(1), S.Just(accounts[0]));

      for(let i = 2; i < 100; i++){
        await testABallot(S.Just(vc), S.Just(i), S.Just(accounts[i]));
      }
      // Woot, tested 99 ballots.

      // check bans
      const addrToBan = accounts[500];
      const _banTx = await vc.banAddress(addrToBan);

      const _isBanned = await vc.bannedAddresses(addrToBan);
      assert.equal(_isBanned, true, "banned addr should be banned");

      const _badBanTx = await vc.banAddress(addrToBan, {from: accounts[4]});
      assertOnlyEvent('Error', _badBanTx);

      let skRevealFailed = false;
      try {
        const _revealSKEarly = await vc.revealSeckey(hexSk);
      } catch (e) {
        skRevealFailed = true;
      }
      assert.equal(skRevealFailed, true, "sk reveal should fail when early");

      // jump to after ballot is closed

      await timeTravel(1000);

      const _revealSK = await vc.revealSeckey(hexSk);
      assertOnlyEvent('SeckeyRevealed', _revealSK);

      const _revealSKBad = await vc.revealSeckey(hexSk, {from: accounts[4]});
      assertOnlyEvent('Error', _revealSKBad);

      const _submitBallotLate = await vc.submitBallot(hexPk, hexPk, {from: accounts[111]});
      assertOnlyEvent('Error', _submitBallotLate);


      await testrpcRevert();
    } catch (err) {
      await testrpcRevert();
      throw err;
    }
  });
  it("pending", function() {
  });
  it("pending2", function() {
  });
});


const testABallot = async function (_vc = S.Nothing, expectVotes = S.Nothing, account = S.Nothing) {
  if (S.isNothing(_vc)) {throw Error("must provide voting contract to `testABallot`")}
  const vc = S.fromMaybe_(() => null, _vc);
  const expectedVotes = S.fromMaybe(1, expectVotes);
  const myAddr = S.fromMaybe(accounts[0], account);
  
  const encBallot = genRandomBytes32();
  const vtrPubkey = genRandomBytes32();

  const _submitBallot = await vc.submitBallot(encBallot, vtrPubkey, {from: myAddr});

  await assertOnlyEvent('SuccessfulVote', _submitBallot);

  const _nVotesRet = await vc.nVotesCast();
  const _ballotId = await vc.voterToBallotID(myAddr);
  const _addr = await vc.associatedAddresses(_ballotId);
  const _pkRet = await vc.associatedPubkeys(_ballotId);
  const _ballotRet = await vc.encryptedBallots(_ballotId);


  assert.equal(_nVotesRet, expectedVotes, "should have " + expectedVotes.toString() + " vote");
  assert.equal(_ballotId.toNumber(), expectedVotes - 1, "should be " + (expectedVotes - 1) + "th ballot");
  assert.equal(_addr, myAddr, "account should match");
  assert.equal(_pkRet, vtrPubkey, "pubkey should match");
  assert.equal(_ballotRet, encBallot, "ballots should match");
}

const assertOnlyEvent = async function (eventName, txResponse) {
  const _eventName = txResponse.logs[0]['event']
  assert.equal(_eventName, eventName, "Event " + eventName + " should be emitted");
}

const sAssertEq = (a, b, msg) => {
  return assert.true(S.equals(a, b), msg);
}


const timeTravel = async function (seconds) {
  const response = web3.currentProvider.send({jsonrpc: "2.0", method: "evm_increaseTime", params: [seconds], id: 0});
  console.log("Time travelled " + seconds + " seconds; new offset: " + response.result)
  return response.result;
}


const getSnapshot = async function() {
  const resp = await web3.currentProvider.send({jsonrpc: "2.0", method: "evm_snapshot", params: [], id: 0});
  return resp.result;
}

const testrpcRevert = async function(snapshot) {
  const args = snapshot ? [snapshot] : []
  return await web3.currentProvider.send({jsonrpc: "2.0", method: "evm_revert", params: args, id: 0});
}
