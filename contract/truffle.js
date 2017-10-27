var TestRPC = require('ethereumjs-testrpc');

const {create, env} = require('sanctuary');
const S = create({checkTypes: true, env});

var provider = TestRPC.provider({port: 34839, accounts: S.map(_ => ({balance: "0xfffffffff999999f"}), S.range(0,200))})

module.exports = {
  networks: {
    development: {
      provider: provider,
      network_id: "*" // Match any network id
    }
  }
};
