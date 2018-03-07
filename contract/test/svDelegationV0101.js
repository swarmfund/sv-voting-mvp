const DCOrig = artifacts.require("./SVDelegation.sol");
const DCv11 = artifacts.require("./SVDelegationV0101.sol");
const R = require('ramda');

require("./testUtils")();

const AsyncPar = require("async-parallel");

const {create, env} = require("sanctuary");
const S = create({checkTypes: true, env});

const bytes32zero =
    "0x0000000000000000000000000000000000000000000000000000000000000000";

const zeroAddr =
    "0x0000000000000000000000000000000000000000";


async function testGlobalDelegation(acc) {
    const dcOrig = await DCOrig.new();
    const dc = await DCv11.new(dcOrig.address);

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    const t1 = acc[4];

    // test before doing anything
    assert.deepEqual([[], []], await dc.findPossibleDelegatorsOf(d1), "await dc.findPossibleDelegatorsOf(d1) returns nothing");

    await dc.setGlobalDelegation(d1, {from: v1});
    assert.equal(d1, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolveDelegation glob good #1");

    assert.deepEqual([[v1], [zeroAddr]], await dc.findPossibleDelegatorsOf(d1), "await dc.findPossibleDelegatorsOf(d1) returns one result");

    await dc.setGlobalDelegation(d2, {from: v1});
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolveDelegation global good #1.1");

    await dc.setTokenDelegation(t1, d1, {from: v1});
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolveDelegation Global good #2");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[3], "resolveDelegation token good #2");

    const votersForD1 = await dc.findPossibleDelegatorsOf(d1);
    const votersForD2 = await dc.findPossibleDelegatorsOf(d2);
    assert.deepEqual([[v1, v1], [zeroAddr, t1]], votersForD1, "possible delegators matches #3 d1");
    assert.deepEqual([[v1], [zeroAddr]], votersForD2, "possible delegators matches #3 d2");
}


const testTokenDelegation = async (acc) => {
    const dcOrig = await DCOrig.new();
    const dc = await DCv11.new(dcOrig.address);

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    const t1 = acc[4];

    await dc.setTokenDelegation(t1, d1, {from: v1});
    assert.equal(d1, (await dc._rawGetTokenDelegation(v1, t1))[3], "rawGetTokenD good");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[3], "resolve token delegation good");
    assert.equal(zeroAddr, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolve token delegation does not carry to global");
    assert.equal(zeroAddr, (await dc.resolveDelegation(d1, t1))[3], "delegate is not a voter - has no delegation");
};


const testDelegationMixed = async (acc) => {
    const dcOrig = await DCOrig.new();
    const dc = await DCv11.new(dcOrig.address);

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    const t1 = acc[4];

    await dc.setTokenDelegation(t1, d1, {from: v1});
    await dc.setGlobalDelegation(d2, {from: v1});

    assert.equal(d1, (await dc._rawGetTokenDelegation(v1, t1))[3], "rawGetTokenD good");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[3], "resolve token delegation good");
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolve token delegation does not carry to global");
    assert.equal(zeroAddr, (await dc.resolveDelegation(d1, t1))[3], "delegate is not a voter - has no delegation");
};

const testMultiDelegations = async (acc) => {
    const dcOrig = await DCOrig.new();
    const dc = await DCv11.new(dcOrig.address);

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    const t1 = acc[4];

    await dc.setGlobalDelegation(d1, {from: v1});
    assert.equal(d1, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolveDelegation Global good D1");

    await dc.setGlobalDelegation(d2, {from: v1});
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolveDelegation Global good D2");

    await dc.setTokenDelegation(t1, d1, {from: v1});
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[3], "resolveDelegation Global good D2");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[3], "resolveDelegation Global good D2");
}


const testBackwardsCompatibility = async (acc) => {
    const dcOrig = await DCOrig.new();
    const dc = await DCv11.new(dcOrig.address);

    const [v1, v2, d1, d2, d3, d4, t1] = acc;

    await dcOrig.setTokenDelegation(t1, d1, {from: v1});
    await dcOrig.setGlobalDelegation(d2, {from: v2});
    assert.equal(d2, (await dcOrig.resolveDelegation(v2, zeroAddr))[1], "resD orig Global good d");
    assert.equal(d1, (await dcOrig.resolveDelegation(v1, t1))[1], "resD orig Token good d");
    assert.equal(d2, (await dc.resolveDelegation(v2, zeroAddr))[3], "resD Global good d");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[3], "resD Token good d");

    // set d3 as delegate for t1 on new SC
    await dc.setTokenDelegation(t1, d3, {from: v1});
    assert.equal(d1, (await dcOrig.resolveDelegation(v1, t1))[1], "resD orig Token good d - on many SC");
    assert.equal(d3, (await dc.resolveDelegation(v1, t1))[3], "resD Token good d - on many SC");

    // try setting a new delegate on orig SC and ensure we don't pick it up on new sc
    await dcOrig.setTokenDelegation(t1, d4, {from: v1});
    assert.equal(d4, (await dcOrig.resolveDelegation(v1, t1))[1], "resD orig Token good d - on many SC #2");
    assert.equal(d3, (await dc.resolveDelegation(v1, t1))[3], "resD Token good d - on many SC #2");

    // repeat above for global
    await dcOrig.setGlobalDelegation(d2, {from: v1});
    assert.equal(d2, (await dcOrig.resolveDelegation(v1, zeroAddr))[1], "resD orig Global good - #3");
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[3], "resD global good - #3");

    await dc.setGlobalDelegation(d3, {from: v1});
    assert.equal(d2, (await dcOrig.resolveDelegation(v1, zeroAddr))[1], "resD orig Global good - #4");
    assert.equal(d3, (await dc.resolveDelegation(v1, zeroAddr))[3], "resD global good - #4");

    await dcOrig.setGlobalDelegation(d4, {from: v1});
    assert.equal(d4, (await dcOrig.resolveDelegation(v1, zeroAddr))[1], "resD orig Global good - #5");
    assert.equal(d3, (await dc.resolveDelegation(v1, zeroAddr))[3], "resD global good - #5");
}


const testRevocation = async (acc) => {
    const dcOrig = await DCOrig.new();
    const dc = await DCv11.new(dcOrig.address);

    const [v1, v2, d1, d2, t1] = acc;

    await dc.setTokenDelegation(t1, d1, {from: v1});
    await dc.setGlobalDelegation(d2, {from: v1});

    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[3], "token delegation matches");
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[3], "token delegation matches");

    await dc.setTokenDelegation(t1, zeroAddr, {from: v1});
    assert.equal(d2, (await dc.resolveDelegation(v1, t1))[3], "token delegation resolves to global delegation after revocation");
}


const testRealV1OnMainnet = async (acc) => {
    if (process.env.DO_MAINNET_DELEGATION_TEST !== "true") {
        console.warn("WARNING: Skipping mainnet delegation test, use 'DO_MAINNET_DELEGATION_TEST=true' to perform this test");
        return;
    }

    const Web3Custom = require('web3');
    const myW3 = new Web3Custom(new Web3Custom.providers.HttpProvider("https://eth-aws-nv-node-02.secure.vote:8545/svDelegationTests"));

    // 0x2c926cc0e63512d23a1921af78204d0de5786537 is NOT the production version of this wallet, but a test instance
    // deployed via SV hotwallet
    const newDCAddr = "0xf71ea2028e3c3fa58df8922eae6f5482123a17d4".toLowerCase();
    const oldDCAddr = "0xd78d4beabfd3054390d10aeb4258dc2d867f5e17".toLowerCase();
    const swmErc20Addr = "0x9e88613418cF03dCa54D6a2cf6Ad934A78C7A17A".toLowerCase();

    // contracts on mainnet
    const origDC = myW3.eth.contract(DCOrig.abi).at(oldDCAddr);
    const newDC = myW3.eth.contract(DCv11.abi).at(newDCAddr);

    const oldLogs = [];

    log("Getting old logs...");

    let origDCFilter = origDC.allEvents({fromBlock: 5000000, toBlock: 5204019}, (e, v) => {
        if (e)
            throw Error("got error getting old logs: " + JSON.stringify(e));
        oldLogs.push(v);
    });

    log("sleeping 5s to give web3 time to get logs...");
    await (new Promise((resolve, reject) => setTimeout(resolve, 5000)));
    origDCFilter.stopWatching();

    assert.equal(46, oldLogs.length, "should have 46 old logs")

    log("Got old logs!");

    const dlgtMap = {};
    const voterToDelegate = {};
    let allVoters = [];
    let allDelegatees = [];
    const addOrInit = (d => (k, v) => {
        if (d[k] !== undefined) {
            d[k].push(v)
        } else {
            d[k] = [v];
        }
    })(dlgtMap);

    R.map(({args}) => {
        voterToDelegate[args.voter] = args.delegate;
    }, R.filter(l => l.args.tokenContract === swmErc20Addr, oldLogs));

    R.map(([v,d]) => {
        addOrInit(d,v);
        allVoters.push(v);
        allDelegatees.push(d);
    }, R.toPairs(voterToDelegate));

    allVoters = R.uniq(allVoters);
    allDelegatees = R.uniq(allDelegatees);

    // test newDC resolves delegates correctly
    let i, j;

    let fromChain;
    // test newDC can successfully run `findPossibleDelegatorsOf` including backwards compatibility
    await AsyncPar.map(allDelegatees, async d => {
        let expVoters = dlgtMap[d];
        let expTokens = new Array(expVoters.length);
        expTokens.fill(swmErc20Addr);
        fromChain = await newDC.findPossibleDelegatorsOf(d);
        try {
            assert.deepEqual(fromChain, [expVoters, expTokens], "possible delegators works for newDC");
        } catch (e) {
            log(`d: ${d}, expV: ${expVoters}, expT: ${expTokens}, fromChain: ${fromChain}`);
            log(`Got error in allDelegates test for ${d}:`);
            log(e);
            throw e;
        }
        log(`Success for delegatee ${d}!`);
    }, 1);

    await AsyncPar.map(allVoters, async v => {
        let d = voterToDelegate[v];
        assert.equal((await newDC.resolveDelegation(v, swmErc20Addr))[3], d, `voter ${v} delegates ${d}`);
        log(`passed assert for resolveDelegation: voter ${v} delegates ${d}`);
    });

    log("done testing mainnet")
}


const testKovanBackwardsCompat = async (acc) => {
    const tc = "0xAA62468E0668Dc9f2d5A145093cdbfa7D84E1668".toLowerCase();
    const v1 = "0xc45797d1a7accc9fb2dcb054aa907f303a0a08f8";
    const v2 = "0xb4be49829b7f70711b399c6cbfc05fcf33ff7abe";

    const d1 = "0xB4bE49829B7f70711B399c6cBfC05FcF33ff7AbE".toLowerCase();
    const d2 = "0xc45797d1a7accc9fb2dcb054aa907f303a0a08f8";
    const d3 = "0x0000000000000000000000000000000000000000";

    const Web3Custom = require('web3');
    const myW3 = new Web3Custom(new Web3Custom.providers.HttpProvider("https://eth-kovan-aws-nv-node-01.secure.vote:8545/svDelegationTests"));

    // 0x2c926cc0e63512d23a1921af78204d0de5786537 is NOT the production version of this wallet, but a test instance
    // deployed via SV hotwallet
    const newDCAddr = "0x8F6F18b9A83E0b42cE69783a8282441BF8F417fc".toLowerCase();
    const oldDCAddr = "0xAA62468E0668Dc9f2d5A145093cdbfa7D84E1668".toLowerCase();

    const newDC = myW3.eth.contract(DCv11.abi).at(newDCAddr);
    const oldDC = myW3.eth.contract(DCOrig.abi).at(oldDCAddr);

    const posVsForD1 = await newDC.findPossibleDelegatorsOf(d1);
    // log(posVsForD1);
    const expectedPosVsForD1 = [[v1, v2, v2], [tc, tc, zeroAddr]];
    const posVsForD2 = await newDC.findPossibleDelegatorsOf(d2);
    const expectPosD2 = [[v1], [tc]];
    // log(posVsForD2);
    const posVsForZero = await newDC.findPossibleDelegatorsOf(zeroAddr);
    const expectPosZero = [[v1], [tc]];
    // log(posVsForZero);
    assert.deepEqual(expectedPosVsForD1, posVsForD1, `pos delegators match expected for ${d1}`);
    assert.deepEqual(expectPosD2, posVsForD2, `pos delegators match expected for ${d2}`);
    assert.deepEqual(expectPosZero, posVsForZero, `pos delegators match expected for ${zeroAddr}`);
}


contract("SVDelegationV0101", function (_accounts) {
    tests = [
        ["simple global delegation", testGlobalDelegation],
        ["simple token delegation", testTokenDelegation],
        ["complex global and token delegation", testDelegationMixed],
        ["multiple delegates in local and global config", testMultiDelegations],
        ["is backwards compatible", testBackwardsCompatibility],
        ["revocations resolve correctly", testRevocation],
        ["test v1 on kovan backwards compat", testKovanBackwardsCompat],
        ["test v1 on mainnet backwards compatibility", testRealV1OnMainnet]
    ];
    S.map(([desc, f]) => it(desc, wrapTest(_accounts, f)), tests);
});
