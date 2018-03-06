const crypto = require('crypto');

module.exports = function () {
    this.genRandomBytes32 = () => {
        return "0x" + crypto.randomBytes(32).toString("hex");
    };

    this.wrapTest = (accounts, f) => {
        return async () => {
            return await f(accounts);
        };
    };

    this.asyncAssertThrow = async (f, msg) => {
        const _msg = msg ? msg.toString() : "";
        let didError = true;
        let res = "nothing returned";
        try {
            res = await f();
            didError = false;
        } catch (e) {
            const invalidJump = e.message.search('invalid JUMP') >= 0;
            const invalidOpCode = e.message.search('invalid opcode') >= 0;
            assert(invalidJump || invalidOpCode, "Expected throw, got '" + e + "' instead.");
        }

        if (!didError) {
            throw Error("Expected error didn't happen: '" + _msg + "'. Instead got: " + JSON.stringify(res) || res);
        }
    };

    this.asyncAssertDoesNotThrow = async (f, msg) => {
        let res = "nothing returned";
        let didError = false;
        let errMsg;
        try {
            res = await f();
        } catch (e) {
            didError = true;
            errMsg = e.message;
        }

        if (didError) {
            throw Error(`Did not expect throw '${msg}' and got error: ${errMsg}`);
        }
        return res;
    };

    const toAsync = f => async (...args) => {
        return new Promise((res, rej) => {
            f(...args, (e, d) => (e ? rej(e) : res(d)));
        });
    };
    this.toAsync = toAsync;

    this.getBalance = toAsync(web3.eth.getBalance);
    this.getBlockNumber = toAsync(web3.eth.getBlockNumber);

    this.log = (...args) => console.log(...args);


    this.mkPromise = f => (...args) => {
        return new Promise((resolve, reject) => {
            f(...args, (err, resp) => {
                err ? reject(err) : resolve(resp);
            })
        })
    };

    this.assertOnlyEvent = function (eventName, txResponse) {
        const _eventName = txResponse.logs[0]["event"];
        assert.equal(eventName, _eventName, "Event " + eventName + " should be emitted");
    }

    this.getEventFromTxR = function(eventName, txR) {
        for (let i = 0; i < txR.logs.length; i++) {
            const l = txR.logs[i];
            if (l.event === eventName) {
                return l
            }
        }
        throw Error("Event " + eventName + " not found!");
    }

};
