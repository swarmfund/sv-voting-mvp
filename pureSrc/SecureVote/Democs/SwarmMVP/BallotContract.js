"use strict"

const Web3 = require('web3');
const TestRPC = require("ethereumjs-testrpc");
const ethUtils = require('ethereumjs-util');
const R = require('ramda');

const web3 = new Web3();

// we run this file from multiple locations so relative paths need some help
try {
    var loadDetails = require('../../bin/solidity/loadContractDetails');
} catch (err) {
    var loadDetails = require('../bin/solidity/loadContractDetails');
}
const deets = loadDetails();
const abi = deets[0];
const bin = deets[1];


var coinbase = "";
var accounts = [];


exports.setWeb3ProviderImpl = function(host) {
    web3.setProvider(new Web3.providers.HttpProvider(host));
    console.log("Set web3 provider to:", host);
    coinbase = web3.eth.coinbase;
    accounts = web3.eth.accounts;
}


exports.getAccountImpl = function(left, right, n) {
    const acc = accounts[n];
    if (acc) {
        return right(acc)
    }
    return left("Account not found");
}


exports.makeSwmVotingContractImpl = function(just, nothing, addr) {
    if (!ethUtils.isValidAddress(addr)) {
        console.log("Invalid address provided for voting contract");
        return nothing;
    }

    try {
        const contract = web3.eth.contract(abi).at(addr);
        return just(contract);
    } catch (err) {
        console.log("WARNING: Got error when creating Web3 voting contract instance:", err);
        return nothing;
    }
    return nothing;
}


exports.makeErc20ContractImpl = function(just, nothing, addr) {
    if (!ethUtils.isValidAddress(addr)) {
        console.log("Invalid address provided for erc20 contract");
        return nothing;
    }

    const erc20Deets = loadDetails('Erc20');
    const erc20Abi = erc20Deets[0]

    try {
        const contract = web3.eth.contract(erc20Abi).at(addr);
        return just(contract);
    } catch (err) {
        console.log("WARNING: Got error when creating Web3 erc20 contract instance:", err);
        return nothing;
    }
    return nothing;
}


exports.getBallotSKImpl = function(just, nothing, contract) {
    const encSK = contract.getEncSeckey();
    if (encSK === "0x0000000000000000000000000000000000000000000000000000000000000000") {
        return nothing;
    } else {
        return just(encSK);
    }
}


const eitherF = function(left, right, contract) {
    return function(prop, args) {
        try {
            const ans = contract[prop].apply(this, args);
            if (ans.s && ans.e && ans.c) {
                return right(ans.toString(10));
            }
            return right(ans);
        } catch (err) {
            return left(JSON.stringify(err));
        }
    }   
}


exports.getBallotPropImpl = function(left, right, prop, args, contract) {
    const runPropWithArgs = eitherF(left, right, contract);
    const ans = runPropWithArgs(prop, args)
    return ans;
}


exports.getBallotPropAsyncImpl = function(_prop, _args, contract) {
    const prop = R.clone(_prop);
    const args = R.clone(_args);
    return function(onErr, onSucc) {
        args.push({gas: 999999, from: coinbase});
        var ballotAsyncCB = function(err, res) {
            if (err) { onErr(err) }
            onSucc(res);
        }
        args.push(ballotAsyncCB);
        contract[prop].apply(null, args);
    }
}


exports.submitBallotImpl = function(accN, encBallot, senderPk, contract) {
    return function(onErr, onSucc) {
        const eb = "0x" + Buffer.from(encBallot).toString('hex');
        const pk = "0x" + Buffer.from(senderPk).toString('hex');
        contract.submitBallot(eb, pk, {from: accounts[accN + 1], gas: 999999}, function(err, txHash){
            if (err) {
                return onErr(err);
            }
            return onSucc(txHash);
        })
    }
}