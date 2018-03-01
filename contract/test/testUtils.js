const crypto = require('crypto');

module.exports = function() {
  this.genRandomBytes32 = () => {
    return "0x" + crypto.randomBytes(32).toString("hex");
  };

  this.wrapTest = (accounts, f) => {
    return async () => {
      return await f(accounts);
    };
  };

  this.asyncAssertThrow = async (f, msg) => {
    const didError = true;
    try {
      const res = await f();
      didError = false;
    } catch (e) {}

    if (!didError) {
      throw Error("Expected error didn't happen: " + msg.toString());
    }
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
};
