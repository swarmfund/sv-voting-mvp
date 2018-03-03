const LGIndex = artifacts.require("./LittleGovIndex.sol");
const LBB = artifacts.require("./LittleBallotBox.sol");

require("./testUtils")();

const AsyncPar = require("async-parallel");

const {create, env} = require("sanctuary");
const S = create({checkTypes: true, env});

const bytes32zero =
    "0x0000000000000000000000000000000000000000000000000000000000000000";

async function testOwner(accounts) {
    const lg = await LGIndex.new();

    assert.equal(await lg.owner(), accounts[0], "owner set");
    assert.equal(await lg.payTo(), accounts[0], "payTo set");

    await asyncAssertThrow(
        () => lg.setPayTo(accounts[1], {from: accounts[1]}),
        "bad user can't change payTo"
    );
    assert.equal(
        await lg.payTo(),
        accounts[0],
        "payTo can't be changed arbitrarily"
    );

    assert.equal(await lg.paymentEnabled(), true, "payment starts false");
    await lg.setPaymentEnabled(false, {from: accounts[0]});
    assert.equal(await lg.paymentEnabled(), false, "payment made false");

    await lg.setPayTo(accounts[10], {from: accounts[0]});
    assert.equal(await lg.payTo(), accounts[10], "payTo changable");

    const dPrice1 = 9876;
    const iPrice1 = 3849;
    await lg.setEth([dPrice1, iPrice1], {from: accounts[0]});
    assert.equal(await lg.democFee(), dPrice1, "eth/democ matches");
    assert.equal(await lg.ballotFee(), iPrice1, "eth/issue matches");

    await lg.setPaymentEnabled(true, {from: accounts[0]});

    // ensure noone can set the price
    await asyncAssertThrow(
        () => lg.setEth([5, 5], {from: accounts[1]}),
        "setEth only by owner"
    );
    await asyncAssertThrow(
        () => lg.initDemoc("some democ", {from: accounts[1]}),
        "initDemoc should fail when payment required with no payment"
    );

    // check payments
    log("mk democ okay paid");
    const balBefore = await getBalance(accounts[1]);
    // log(await getBlockNumber())
    const democId_ = await lg.initDemoc("some democ", {
        from: accounts[1],
        value: dPrice1 + 938,
        gasPrice: 0
    });
    // log(await getBlockNumber())
    log("democCreationTx: ", democId_.tx, "---   nLogs:", democId_.logs.length);
    const balAfter = await getBalance(accounts[1]);
    log("balances", balBefore.toString(), balAfter.toString());
    assert.isTrue(
        balBefore.minus(dPrice1).eq(balAfter),
        "payment should be accurate and remainder refunded // before: " +
        balBefore.toString() +
        " // after: " +
        balAfter.toString()
    );
    log("init done!");
    const democId = await lg.democList(0);
    log("democId", democId);

    await lg.setPaymentEnabled(false, {from: accounts[0]});
    assert.equal(await lg.paymentEnabled(), false, "payment null now");

    // check pay okay but still free fails
    log("mk democ not okay");
    await lg.setPaymentEnabled(true, {from: accounts[0]});
    await asyncAssertThrow(
        () => lg.initDemoc("free lunch democ (bad)", {from: accounts[1]}),
        "no free lunch (democ)"
    );
    await asyncAssertThrow(
        () =>
            lg.addBallot(democId, democId, accounts[1], {
                from: accounts[1]
            }),
        "no free lunch (issue)"
    );
    log("bad ballots over, confirming we can still make them...")

    const lbb = await LBB.new(democId, [0, 0], [true, false]);
    log("created LBB to work with... adding a ballot");

    // make sure we can still pay for a ballot though
    await lg.addBallot(democId, democId, lbb.address, {from: accounts[1], value: iPrice1});
    log("addballot okay paid");
    await lg.addBallot(democId, democId, lbb.address, {
        from: accounts[1],
        value: iPrice1,
        gasPrice: 0
    });
    log("ballot added!");
    await lg.setPaymentEnabled(false, {from: accounts[0]});
    log("add ballot okay free");
    await lg.addBallot(democId, democId, lbb.address, {
        from: accounts[1]
    });

    log("enable payments")
    // whitelist for issues
    await lg.setPaymentEnabled(true, {from: accounts[0]});
    await asyncAssertThrow(
        () =>
            lg.addBallot(democId, democId, accounts[1], {
                from: accounts[1]
            }),
        "no free lunch (issue)"
    );
    log("give accounts[1] whitelist access")
    await lg.setWhitelistBallot(accounts[1], true);
    log("accounts 1 makes ballot with no payment")
    await lg.addBallot(democId, democId, lbb.address, {
        from: accounts[1]
    });

    // check whitelist for democs
    await asyncAssertThrow(
        () => lg.initDemoc("free lunch democ (bad)", {from: accounts[1]}),
        "no free lunch democ"
    );
    log("give accounts[1] democ whitelist")
    await lg.setWhitelistDemoc(accounts[1], true);
    log("confirm whitelist works")
    await lg.initDemoc("actually free lunch (good)", {from: accounts[1]});

    // make sure whitelists are still blocking ppl
    await asyncAssertThrow(
        () =>
            lg.addBallot(democId, democId, bytes32zero, accounts[1], {
                from: accounts[1]
            }),
        "no free lunch (issue)"
    );

    log("confirm a[1] is admin of", democId);
    assert.equal(accounts[1], (await lg.getDemocInfo(democId))[1], "admin should be accounts[1]");
    await asyncAssertThrow(
        () => lg.initDemoc("free lunch democ (bad)", {from: accounts[2]}),
        "no free lunch democ"
    );

    log("try and deploy a ballot through the index")
    // check we can deploy a new ballot
    await lg.setPaymentEnabled(false, {from: accounts[0]})
    // get some info before hand and validate it
    const nBallotsPre = await lg.nBallots(democId);
    log("got pre-deploy nBallots", nBallotsPre.toNumber());
    await asyncAssertThrow(() => lg.getNthBallot(democId, nBallotsPre), "nonexistant democ will throw");

    log("confirmed there is not ballot there yet - deploying now")
    await lg.deployBallot(democId, democId, bytes32zero, [0, 20000000000], [false, false], {from: accounts[1]});
    log("deployed...")
    const newBallot = await lg.getNthBallot(democId, nBallotsPre);
    log("got new ballot!", newBallot);
    assert.notEqual(bytes32zero, newBallot[0], "n+1th ballot should now not be zeros");

    // check that we can read it and all that
    const newBallotVC = LBB.at(newBallot[2]);
    assert.equal(newBallot[0], await newBallotVC.specHash(), "spec hashes should match as reported by LGI and LBB");
    assert.equal(democId, await newBallotVC.specHash(), "spec hashe should match what we gave it");
}


const testPayments = async (acc) => {
    const admin = acc[0];
    const userPaid = acc[1];
    const userFree = acc[2];

    const [democPrice, ballotPrice] = S.map(a => web3.toWei(a, 'ether'), [0.05, 0.01]);

    const lg = await LGIndex.new();
    await lg.setEth([democPrice, ballotPrice], {from: admin});
    await lg.setWhitelistDemoc(userFree, true, {from: admin});
    await lg.setWhitelistBallot(userFree, true, {from: admin});

    await asyncAssertThrow(() => lg.initDemoc("userPaidFail", {from: userPaid}), "userPaid must pay");

    // test making a payment and getting change
    const _userPaidBalPre = await getBalance(userPaid);
    const _democInitPaid = await lg.initDemoc("userPaidGood", {from: userPaid, value: democPrice + 1337, gasPrice: 0});
    const _userPaidBalPost = await getBalance(userPaid);
    assert.isTrue(_userPaidBalPre.eq(_userPaidBalPost.add(democPrice)), "extra wei should be refunded");

    // assert event and get democId
    console.log("democInitPaid", _democInitPaid);
    assertOnlyEvent("PaymentMade", _democInitPaid);
    const _dInitEvent = getEventFromTxR("DemocInit", _democInitPaid);
    const democId = _dInitEvent.args.democHash;

    // test a payment for democId
    await asyncAssertThrow(() => lg.deployBallot(democId, democId, democId, [0, 0], [true, true], {from: userPaid}), "userPaid can't publish issues for free");
    const _ballotTxR = await lg.deployBallot(democId, democId, democId, [0, 0], [true, true], {
        from: userPaid,
        value: ballotPrice
    });
    const _ballotDeployE = getEventFromTxR("BallotInit", _ballotTxR);

    // test userFree can do this for free
    const _democFreeE = getEventFromTxR("DemocInit", await lg.initDemoc("free democ", {from: userFree}));
    const _freeDemocId = _democFreeE.args.democHash;
    await lg.deployBallot(_freeDemocId, _freeDemocId, _freeDemocId, [0, 0], [true, true], {from: userFree});
}


contract("LittleGovIndex", function (_accounts) {
    tests = [
        ["end-to-end-ish", testOwner],
        ["payment amounts", testPayments],

    ];
    S.map(([desc, f]) => it(desc, wrapTest(_accounts, f)), tests);
});
