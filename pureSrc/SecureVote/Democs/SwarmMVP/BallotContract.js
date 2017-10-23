"use strict"

const Web3 = require('web3');
const TestRPC = require("ethereumjs-testrpc");
const ethUtils = require('ethereumjs-util');

const web3 = new Web3();

// we run this file from multiple locations so relative paths need some help
var loadDetails = function(){throw Error("Not implemented")};
try {
    loadDetails = require('../../bin/solidity/loadContractDetails');
} catch (err) {
    loadDetails = require('../bin/solidity/loadContractDetails');
}
const deets = loadDetails();
const abi = deets[0];
const bin = deets[1];


exports.setWeb3ProviderImpl = function(host) {
    web3.setProvider(new Web3.providers.HttpProvider(host));
}


exports.getAccountImpl = function(left, right, n) {
    const acc = web3.eth.accounts[n];
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
        console.log("WARNING: Got error when creating Web3 contract instance:", err);
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
            console.log("eitherF got ans", ans)
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
    console.log("Running", prop, "args:", args)
    const ans = runPropWithArgs(prop, args)
    return ans;
}


exports.getBallotPropAsyncImpl = function(prop, args, contract) {
    return function(onErr, onSucc) {
        console.log('ballot async', prop, args);
        args.push({gas: 999999, from: web3.eth.coinbase});
        args.push(function(err, res) {
            console.log('ballot async got', err, res);
            if (err) { onErr(err) }
            onSucc(res);
        })
        contract[prop].apply(this, args);
    }
}


exports.submitBallotImpl = function(accN, encBallot, senderPk, contract) {
    return function(onErr, onSucc) {
        const eb = "0x" + Buffer.from(encBallot).toString('hex');
        const pk = "0x" + Buffer.from(senderPk).toString('hex');
        contract.submitBallot(eb, pk, {from: web3.eth.accounts[accN + 1], gas: 999999}, function(err, txHash){
            if (err) {
                return onErr(err);
            }
            return onSucc(txHash);
        })
    }
}