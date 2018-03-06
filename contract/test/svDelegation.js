const DCOrig = artifacts.require("./SVDelegation.sol");

require("./testUtils")();

const AsyncPar = require("async-parallel");

const {create, env} = require("sanctuary");
const S = create({checkTypes: true, env});

const bytes32zero =
    "0x0000000000000000000000000000000000000000000000000000000000000000";

const zeroAddr =
    "0x0000000000000000000000000000000000000000";

async function testGlobalDelegation(acc) {
    const dc = await DCOrig.new();

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    await dc.setGlobalDelegation(d1, {from: v1});
    assert.equal(d1, (await dc._rawGetGlobalDelegation(v1))[1], "rawGetGlobal good");
    assert.equal(d1, (await dc.resolveDelegation(v1, zeroAddr))[1], "resolveDelegation Global good");
    assert.equal(zeroAddr, (await dc.resolveDelegation(d1, zeroAddr))[1], "delegate is not a voter - has no delegation");
}


const testTokenDelegation = async (acc) => {
    const dc = await DCOrig.new();

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    const t1 = acc[4];

    await dc.setTokenDelegation(t1, d1, {from: v1});
    assert.equal(d1, (await dc._rawGetTokenDelegation(v1, t1))[1], "rawGetTokenD good");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[1], "resolve token delegation good");
    assert.equal(zeroAddr, (await dc.resolveDelegation(v1, zeroAddr))[1], "resolve token delegation does not carry to global");
    assert.equal(zeroAddr, (await dc.resolveDelegation(d1, t1))[1], "delegate is not a voter - has no delegation");
};

const testDelegationMixed = async (acc) => {
    const dc = await DCOrig.new();

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    const t1 = acc[4];

    await dc.setTokenDelegation(t1, d1, {from: v1});
    await dc.setGlobalDelegation(d2, {from: v1});

    assert.equal(d1, (await dc._rawGetTokenDelegation(v1, t1))[1], "rawGetTokenD good");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[1], "resolve token delegation good");
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[1], "resolve token delegation does not carry to global");
    assert.equal(zeroAddr, (await dc.resolveDelegation(d1, t1))[1], "delegate is not a voter - has no delegation");
};

const testMultiDelegations = async (acc) => {
    const dc = await DCOrig.new();

    const v1 = acc[0];
    const v2 = acc[1];

    const d1 = acc[2];
    const d2 = acc[3];

    const t1 = acc[4];

    await dc.setGlobalDelegation(d1, {from: v1});
    assert.equal(d1, (await dc.resolveDelegation(v1, zeroAddr))[1], "resolveDelegation Global good D1");

    await dc.setGlobalDelegation(d2, {from: v1});
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[1], "resolveDelegation Global good D2");

    await dc.setTokenDelegation(t1, d1, {from: v1});
    assert.equal(d2, (await dc.resolveDelegation(v1, zeroAddr))[1], "resolveDelegation Global good D2");
    assert.equal(d1, (await dc.resolveDelegation(v1, t1))[1], "resolveDelegation Global good D2");
}

contract("SVDelegation", function (_accounts) {
    tests = [
        ["simple global delegation", testGlobalDelegation],
        ["simple token delegation", testTokenDelegation],
        ["complex global and token delegation", testDelegationMixed],
        ["multiple delegates in local and global config", testMultiDelegations]
    ];
    S.map(([desc, f]) => it(desc, wrapTest(_accounts, f)), tests);
});
