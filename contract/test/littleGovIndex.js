var LGIndex = artifacts.require("./LittleGovIndex.sol");

require('./testUtils')();

const AsyncPar = require('async-parallel');

const {create, env} = require('sanctuary');
const S = create({checkTypes: true, env});

const bytes32zero = "0x0000000000000000000000000000000000000000000000000000000000000000";


async function testOwner(accounts) {
  const lg = await LGIndex.new();

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


contract('LittleGovIndex', function(_accounts) {
  it("should pass owner tests", wrapTest(_accounts, testOwner));
  // it("should instantiate correctly", wrapTest(testInstantiation));
  // it("pending2", function() {
  // });

});
