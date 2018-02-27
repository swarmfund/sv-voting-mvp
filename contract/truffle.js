var TestRPC = require("ethereumjs-testrpc");

const { create, env } = require("sanctuary");
const S = create({ checkTypes: true, env });

var provider = TestRPC.provider({
  port: 34839,
  accounts: S.map(_ => ({ balance: "0xfffffffffffffffff" }), S.range(0, 20))
});

module.exports = {
  networks: {
    development: {
      provider: provider,
      // host: "localhost",
      // port: 8545,
      gas: 4500000,
      network_id: "*" // Match any network id
    }
  }
};
