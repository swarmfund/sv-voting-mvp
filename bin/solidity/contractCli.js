const repl = require("repl");
const context = repl.start("> ").context;


const loadDetails = require('./loadContractDetails');
const Web3 = require('web3');
context.R = require('ramda');

context.web3 = new Web3(new Web3.providers.HttpProvider('http://localhost:8545'));

[context.abi, context.bin] = loadDetails();

context.contract = context.web3.eth.contract(context.abi);

context.ballotEncPubkey = "0x16a61c250644172ad3bbe2445bd903a8ce3a864744f7870e53195fd08088ee27";
context.ballotEncSeckey = "0xf28fa1cc88ef3e2fca363e1d19e7df3ae4aa3f0757513b695bd6ed05ee9aa995";


console.log("These variables are available:");
console.log("R, web3, abi, bin, contract, ballotEncPubkey, ballotEncSeckey");
