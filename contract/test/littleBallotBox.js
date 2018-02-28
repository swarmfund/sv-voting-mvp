var LittleBallotBox = artifacts.require("./LittleBallotBox.sol");

require("./testUtils")();

var naclJs = require("js-nacl");
var crypto = require("crypto");

const AsyncPar = require("async-parallel");

const { create, env } = require("sanctuary");
const S = create({ checkTypes: true, env });

const bytes32zero = "0x0000000000000000000000000000000000000000000000000000000000000000";

var hexSk = "0xcd9d715f05a4fce8acf3339fd5ee8549c1899c52e4b32da07cffcd91a29ad976";
var hexPk = "0xba781ed1006bd7694282a210485265f1c503f4e6721858b4269ae6d745f7bb4b";
var specHash = "0x418781fb172c2a30c072d58628f5df3f12052a0b785450fb0105f1b98b504561";

async function testEarlyBallot(accounts) {
    var startTime = Math.round(Date.now() / 1000) + 2;
    var endTime = startTime + 600;
    var shortEndTime = 0;

    const vc = await LittleBallotBox.new(specHash, startTime, endTime, true, true);
    await asyncAssertThrow(() => vc.submitBallotWithPk(hexPk, hexPk, { from: accounts[5] }), "should throw on early ballot");
}

async function testEncryptionBranching(accounts) {
    var startTime = Math.round(Date.now() / 1000) - 2;
    var endTime = startTime + 600;
    var shortEndTime = 0;

    /* ENCRYPTION */

    // best BB with enc
    const vcEnc = await LittleBallotBox.new(specHash, startTime, endTime, true, true);

    // check we're using enc
    assert.isTrue(await vcEnc.useEncryption(), "encryption should be enabled");

    // check submissions with enc
    asyncAssertThrow(() => vcEnc.submitBallotNoPk(hexPk), "throw when not using encryption");
    assert.equal(await vcEnc.nVotesCast(), 0, "no votes yet");
    const _wEnc = await vcEnc.submitBallotWithPk(hexPk, specHash);
    assert.equal(await vcEnc.nVotesCast(), 1, "1 vote");
    assertOnlyEvent("SuccessfulPkVote", _wEnc);
    assert.equal(await vcEnc.ballotMap(0), hexPk);
    assert.equal(await vcEnc.associatedPubkeys(0), specHash);
    assert.equal(await vcEnc.associatedAddresses(0), accounts[0]);

    /* NO ENCRYPTION */

    // create ballot box with no enc
    const vcNoEnc = await LittleBallotBox.new(specHash, startTime, endTime, false, true);

    // assert useEnc is false with no enc
    assert.isFalse(await vcNoEnc.useEncryption(), "encryption should be disabled");
    // test ballot submissions w no enc
    const _noEnc = await vcNoEnc.submitBallotNoPK(hexSk);
    assertOnlyEvent("SuccessfulVote", _noEnc);
    assert.equal(await vcNoEnc.ballotMap(0), hexSk);
    assert.equal(await vcNoEnc.associatedPubkeys(0), bytes32zero);
    assert.equal(await vcNoEnc.associatedAddresses(0), accounts[0]);

    assert.equal(await vcEnc.nVotesCast(), 1, "1 vote");
    await asyncAssertThrow(() => vcNoEnc.submitBallotWithPk(hexSk, hexSk), "should throw with enc disabled");
    assert.equal(await vcEnc.nVotesCast(), 1, "still only 1 vote");
}

async function testInstantiation(accounts) {
    var startTime = Math.round(Date.now() / 1000) - 2;
    var endTime = startTime + 600;
    var shortEndTime = 0;

    const vc = await LittleBallotBox.new(specHash, startTime, endTime, true, true);

    log(accounts[0]);
    assert.equal(await vc.owner(), accounts[0], "Owner must be set on launch.");

    assert.equal(await vc.specHash(), specHash, "specHash should be equal");

    const _startTime = await vc.startTime();
    assert.equal(startTime, _startTime, "startTime matches");

    const _endTime = await vc.endTime();
    assert.equal(endTime, _endTime, "endTime matches");

    const _testMode = await vc.testMode();
    assert.equal(_testMode, true, "We should be in test mode");

    const _nVotes = await vc.nVotesCast();
    assert.equal(_nVotes, 0, "Should have no votes at start");

    const _sk = await vc.ballotEncryptionSeckey();
    assert.equal(_sk, bytes32zero, "ballot enc key should be zeros before reveal");

    //// ASSERTIONS FOR INSTANTIATION COMPLETE

    // await sleep(startTime - (Date.now() / 1000) + 1.1)

    // try a single ballot first
    await testABallot(accounts)(S.Just(vc), S.Just(accounts[0]));

    const nVotes = 10;
    try {
        // now a bunch
        await AsyncPar.map(S.range(0, nVotes), async i => {
            return await testABallot(accounts)(S.Just(vc), S.Just(accounts[i]));
        });
        // Woot, tested 98 ballots.
    } catch (err) {
        console.log(err.message);
        for (var item of err.list) {
            console.log(item.message);
        }
        throw err;
    }

    assert.equal((await vc.nVotesCast()).toNumber(), nVotes + 1, "should have cast " + (nVotes + 1).toString() + " votes thus far");

    await asyncAssertThrow(() => vc.revealSeckey(hexSk), "should throw on early reveal");

    // jump to after ballot is closed
    // note: originally used testrpc's evm_increaseTime RPC call, but you can't go backwards or undo, so it screws up other test 👿
    await vc.setEndTime(0);

    await asyncAssertThrow(() => vc.revealSeckey(hexSk, { from: accounts[4] }), "throw on bad reveal from non-admin");

    const _revealSK = await vc.revealSeckey(hexSk);
    assertOnlyEvent("SeckeyRevealed", _revealSK);

    await asyncAssertThrow(() => vc.submitBallotWithPk(hexPk, hexPk, { from: accounts[4] }), "late ballot throws");
}

async function testTestMode(accounts) {
    var vc = await LittleBallotBox.new(specHash, 0, 1, false, false);
    await asyncAssertThrow(() => vc.setEndTime(0), "throws on set end time when not in testing");
}

const testABallot = accounts => async (_vc = S.Nothing, account = S.Nothing) => {
    if (S.isNothing(_vc)) {
        throw Error("must provide voting contract to `testABallot`");
    }
    const vc = S.fromMaybe_(() => null, _vc);
    const myAddr = S.fromMaybe(accounts[0], account);

    const encBallot = genRandomBytes32();
    const vtrPubkey = genRandomBytes32();

    const _submitBallotWithPk = await vc.submitBallotWithPk(encBallot, vtrPubkey, {
        from: myAddr
    });

    await assertOnlyEvent("SuccessfulPkVote", _submitBallotWithPk);

    // const _nVotesRet = await vc.nVotesCast();
    const _ballotId = await vc.voterToBallotID(myAddr);
    const _addr = await vc.associatedAddresses(_ballotId);
    const _pkRet = await vc.associatedPubkeys(_ballotId);
    const _ballotRet = await vc.ballotMap(_ballotId);

    // note: these two tests do not work in parallel - disabled
    // assert.equal(_nVotesRet.toNumber(), expectedVotes, "should have " + expectedVotes.toString() + " vote");
    // assert.equal(_ballotId.toNumber(), expectedVotes - 1, "should be " + (expectedVotes - 1) + "th ballot");
    assert.equal(_addr, myAddr, "account should match");
    assert.equal(_pkRet, vtrPubkey, "pubkey should match");
    assert.equal(_ballotRet, encBallot, "ballots should match");

    return true;
};

async function assertOnlyEvent(eventName, txResponse) {
    const _eventName = txResponse.logs[0]["event"];
    assert.equal(eventName, _eventName, "Event " + eventName + " should be emitted");
}

function sAssertEq(a, b, msg) {
    return assert.true(S.equals(a, b), msg);
}

async function timeTravel(seconds) {
    const response = web3.currentProvider.send({
        jsonrpc: "2.0",
        method: "evm_increaseTime",
        params: [seconds],
        id: 0
    });
    console.log("Time travelled " + seconds + " seconds; new offset: " + response.result);
    return response.result;
}

async function getSnapshot() {
    const resp = await web3.currentProvider.send({
        jsonrpc: "2.0",
        method: "evm_snapshot",
        params: [],
        id: 0
    });
    return resp.result;
}

async function testrpcRevert(snapshot) {
    const args = snapshot ? [snapshot] : [];
    return await web3.currentProvider.send({
        jsonrpc: "2.0",
        method: "evm_revert",
        params: args,
        id: 0
    });
}

contract("LittleBallotBox", function(_accounts) {
    it("should instantiate correctly", wrapTest(_accounts, testInstantiation));
    it("should enforce encryption based on PK submitted", wrapTest(_accounts, testEncryptionBranching));
    it("should not allow testing functions if testing mode is false", wrapTest(_accounts, testTestMode));
    it("should throw on early ballot", wrapTest(_accounts, testEarlyBallot));
});
