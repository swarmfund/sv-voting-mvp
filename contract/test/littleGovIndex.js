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
        "can't change payTo"
    );
    assert.equal(
        await lg.payTo(),
        accounts[0],
        "payTo can't be changed arbitrarily"
    );

    assert.equal(await lg.paymentEnabled(), false, "payment starts false");
    await lg.setPaymentEnabled(true, {from: accounts[0]});
    assert.equal(await lg.paymentEnabled(), true, "payment made true");

    await lg.setPayTo(accounts[10], {from: accounts[0]});
    assert.equal(await lg.payTo(), accounts[10], "payTo changable");

    const dPrice1 = 9876;
    const iPrice1 = 3849;
    await lg.setEth(dPrice1, iPrice1, {from: accounts[0]});
    assert.equal(await lg.requiredEthForDemoc(), dPrice1, "eth/democ matches");
    assert.equal(await lg.requiredEthForIssue(), iPrice1, "eth/issue matches");

    // ensure noone can set the price
    await asyncAssertThrow(
        () => lg.setEth(5, 5, {from: accounts[1]}),
        "setEth only by owner"
    );
    await asyncAssertThrow(
        () => lg.initDemoc("some democ", {from: accounts[1]}),
        "initDemoc should fail"
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
    await lg.setWhitelistIssue(accounts[1], true);
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

contract("LittleGovIndex", function (_accounts) {
    it("whole test suite", wrapTest(_accounts, testOwner));
});
