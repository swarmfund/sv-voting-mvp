var LGIndex = artifacts.require("./LittleGovIndex.sol");

const AsyncPar = require('async-parallel');

const {create, env} = require('sanctuary');
const S = create({checkTypes: true, env});

const bytes32zero = "0x0000000000000000000000000000000000000000000000000000000000000000";


const genRandomBytes32 = () => {
  return "0x" + crypto.randomBytes(32).toString('hex');
}


function wrapTest(accounts, f) {
  return async () => {
    return await f(accounts, LGIndex);
  }
}


async function asyncAssertThrow(f, msg) {
  const didError = true;
  try{
    const res = await f();
    didError = false;
  } catch (e) {
  }

  if (!didError) {
    throw Error("Expected error didn't happen: " + msg.toString())
  }
}


const toAsync = (f) => async(...args) => {
  return new Promise((res, rej) => {
    f(...args, (e, d) => e ? rej(e) : res(d))
  })
}


const getBalance = toAsync(web3.eth.getBalance)
const getBlockNumber = toAsync(web3.eth.getBlockNumber)


const ERR_REVERT = 'VM Exception while processing transaction: revert';
const ERR_OP_CODE = 'VM Exception while processing transaction: invalid opcode';


const log = (...args) => console.log(...args);


async function testOwner(accounts, contractFactory) {
  const lg = await contractFactory.new();

  assert.equal(await lg.owner(), accounts[0], "owner set");
  assert.equal(await lg.payTo(), accounts[0], "payTo set");

  await asyncAssertThrow(() => lg.setPayTo(accounts[1], {from: accounts[1]}), "can't change payTo")
  assert.equal(await lg.payTo(), accounts[0], "payTo can't be changed arbitrarily")

  assert.equal(await lg.paymentEnabled(), false, "payment starts false")
  await lg.setPaymentEnabled(true, {from: accounts[0]})
  assert.equal(await lg.paymentEnabled(), true, "payment made true")

  await lg.setPayTo(accounts[10], {from: accounts[0]});
  assert.equal(await lg.payTo(), accounts[10], "payTo changable")

  const dPrice1 = 9876;
  const iPrice1 = 3849;
  await lg.setEth(dPrice1, iPrice1, {from: accounts[0]});
  assert.equal(await lg.requiredEthForDemoc(), dPrice1, "eth/democ matches");
  assert.equal(await lg.requiredEthForIssue(), iPrice1, "eth/issue matches");

  // ensure noone can set the price
  await asyncAssertThrow(() => lg.setEth(5,5, {from: accounts[1]}), "setEth only by owner");
  await asyncAssertThrow(() => lg.initDemoc("some democ", {from: accounts[1]}), "initDemoc should fail");

  // check payments
  log("mk democ okay paid")
  const balBefore = await getBalance(accounts[1]);
  // log(await getBlockNumber())
  const democId_ = await lg.initDemoc("some democ", {from: accounts[1], value: dPrice1 + 938, gasPrice: 0});
  // log(await getBlockNumber())
  log("democCreationTx: ", democId_.tx, "---   nLogs:", democId_.logs.length)
  const balAfter = await getBalance(accounts[1]);
  log("balances", balBefore.toString(), balAfter.toString())
  assert.isTrue(balBefore.minus(dPrice1).eq(balAfter), "payment should be accurate and remainder refunded // before: " + balBefore.toString() + " // after: " + balAfter.toString());
  log("init done!")
  const democId = await lg.democList(0);
  log("democId", democId);

  await lg.setPaymentEnabled(false, {from: accounts[0]})
  assert.equal(await lg.paymentEnabled(), false, "payment null now");

  // check pay okay but still free fails
  log("mk democ not okay")
  await lg.setPaymentEnabled(true, {from: accounts[0]});
  await asyncAssertThrow(() => lg.initDemoc("free lunch democ (bad)", {from: accounts[1]}), "no free lunch (democ)")
  await asyncAssertThrow(() => lg.addBallot(democId, democId, bytes32zero, accounts[1], {from: accounts[1]}), "no free lunch (issue)")
  log("addballot okay paid")
  await lg.addBallot(democId, democId, bytes32zero, accounts[1], {from: accounts[1], value: iPrice1, gasPrice: 0})
  log("ballot added")
  await lg.setPaymentEnabled(false, {from: accounts[0]});
  log("add ballot okay free")
  await lg.addBallot(democId, democId, bytes32zero, accounts[1], {from: accounts[1]});

  // whitelist for issues
  await lg.setPaymentEnabled(true, {from: accounts[0]});
  await asyncAssertThrow(() => lg.addBallot(democId, democId, bytes32zero, accounts[1], {from: accounts[1]}), "no free lunch (issue)")
  await lg.setWhitelistIssue(accounts[1], true);
  await lg.addBallot(democId, democId, bytes32zero, accounts[1], {from: accounts[1]});

  // check whitelist for democs
  await asyncAssertThrow(() => lg.initDemoc("free lunch democ (bad)", {from: accounts[1]}), "no free lunch democ");
  await lg.setWhitelistDemoc(accounts[1], true);
  await lg.initDemoc("actually free lunch (good)", {from: accounts[1]});

  // make sure whitelists are still blocking ppl
  await asyncAssertThrow(() => lg.addBallot(democId, democId, bytes32zero, accounts[1], {from: accounts[2]}), "no free lunch (issue)")
  await asyncAssertThrow(() => lg.initDemoc("free lunch democ (bad)", {from: accounts[2]}), "no free lunch democ");


}


// async function testTestMode(accounts, contractFactory) {
//   var startTime = Math.round(Date.now() / 1000);
//   var endTime = startTime + 600;
//   var shortEndTime = 0;

//   var vc = await contractFactory.new(startTime, endTime, hexPk, false, "", "", "", "", "");

//   const _setEndTimeTx = await vc.setEndTime(0);
//   assertOnlyEvent('Error', _setEndTimeTx);

//   const _banAddrTx = await vc.banAddress(accounts[1]);
//   assertOnlyEvent('Error', _banAddrTx);
// }










// async function testABallot(_vc = S.Nothing, account = S.Nothing) {
//   if (S.isNothing(_vc)) {throw Error("must provide voting contract to `testABallot`")}
//   const vc = S.fromMaybe_(() => null, _vc);
//   const myAddr = S.fromMaybe(accounts[0], account);

//   const encBallot = genRandomBytes32();
//   const vtrPubkey = genRandomBytes32();

//   const _submitBallot = await vc.submitBallot(encBallot, vtrPubkey, {from: myAddr});

//   await assertOnlyEvent('SuccessfulVote', _submitBallot);

//   // const _nVotesRet = await vc.nVotesCast();
//   const _ballotId = await vc.voterToBallotID(myAddr);
//   const _addr = await vc.associatedAddresses(_ballotId);
//   const _pkRet = await vc.associatedPubkeys(_ballotId);
//   const _ballotRet = await vc.encryptedBallots(_ballotId);

//   // note: these two tests do not work in parallel - disabled
//   // assert.equal(_nVotesRet.toNumber(), expectedVotes, "should have " + expectedVotes.toString() + " vote");
//   // assert.equal(_ballotId.toNumber(), expectedVotes - 1, "should be " + (expectedVotes - 1) + "th ballot");
//   assert.equal(_addr, myAddr, "account should match");
//   assert.equal(_pkRet, vtrPubkey, "pubkey should match");
//   assert.equal(_ballotRet, encBallot, "ballots should match");

//   return true;
// }

// async function assertOnlyEvent(eventName, txResponse) {
//   const _eventName = txResponse.logs[0]['event']
//   assert.equal(eventName, _eventName, "Event " + eventName + " should be emitted");
// }

// function sAssertEq(a, b, msg) {
//   return assert.true(S.equals(a, b), msg);
// }


// async function timeTravel(seconds) {
//   const response = web3.currentProvider.send({jsonrpc: "2.0", method: "evm_increaseTime", params: [seconds], id: 0});
//   console.log("Time travelled " + seconds + " seconds; new offset: " + response.result)
//   return response.result;
// }


// async function getSnapshot() {
//   const resp = await web3.currentProvider.send({jsonrpc: "2.0", method: "evm_snapshot", params: [], id: 0});
//   return resp.result;
// }

// async function testrpcRevert(snapshot) {
//   const args = snapshot ? [snapshot] : []
//   return await web3.currentProvider.send({jsonrpc: "2.0", method: "evm_revert", params: args, id: 0});
// }



// function sleep(s) {return new Promise(res => setTimeout(res, s * 1000))};



contract('LittleGovIndex', function(_accounts) {
  it("should pass owner tests", wrapTest(_accounts, testOwner));
  // it("should instantiate correctly", wrapTest(testInstantiation));
  // it("pending2", function() {
  // });

});
