"use strict"

const Web3 = require('web3');
const ethUtils = require('ethereumjs-util');
const split = require('ramda/src/split');
const clone = require('ramda/src/clone');

const web3 = new Web3();


const abi = [{"constant":true,"inputs":[],"name":"nVotesCast","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getBallotOptions","outputs":[{"name":"","type":"bytes32[5]"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"encryptedBallot","type":"bytes32"},{"name":"senderPubkey","type":"bytes32"}],"name":"submitBallot","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"ballotEncryptionPubkey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"ballotEncryptionSeckey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"endTime","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getEncSeckey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"}],"name":"voterToBallotID","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getBallotOptNumber","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"}],"name":"bannedAddresses","outputs":[{"name":"","type":"bool"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"uint256"}],"name":"associatedAddresses","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"swarmFundAddress","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getEncPubkey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"uint256"}],"name":"encryptedBallots","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"startTime","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_addr","type":"address"}],"name":"banAddress","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"owner","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"uint256"}],"name":"optionHashes","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"uint256"}],"name":"associatedPubkeys","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_secKey","type":"bytes32"}],"name":"revealSeckey","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[{"name":"newEndTime","type":"uint256"}],"name":"setEndTime","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"testMode","outputs":[{"name":"","type":"bool"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[{"name":"_startTime","type":"uint256"},{"name":"_endTime","type":"uint256"},{"name":"_encPK","type":"bytes32"},{"name":"enableTesting","type":"bool"},{"name":"opt1","type":"string"},{"name":"opt2","type":"string"},{"name":"opt3","type":"string"},{"name":"opt4","type":"string"},{"name":"opt5","type":"string"}],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"creator","type":"address"},{"indexed":false,"name":"start","type":"uint256"},{"indexed":false,"name":"end","type":"uint256"},{"indexed":false,"name":"encPubkey","type":"bytes32"}],"name":"CreatedBallot","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"voter","type":"address"},{"indexed":false,"name":"reason","type":"string"}],"name":"FailedVote","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"voter","type":"address"},{"indexed":false,"name":"ballot","type":"bytes32"},{"indexed":false,"name":"pubkey","type":"bytes32"}],"name":"SuccessfulVote","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"secretKey","type":"bytes32"}],"name":"SeckeyRevealed","type":"event"},{"anonymous":false,"inputs":[],"name":"TestingEnabled","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"error","type":"string"}],"name":"Error","type":"event"}];
const legacyAbi = [{"constant":true,"inputs":[],"name":"nVotesCast","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getBallotOptions","outputs":[{"name":"","type":"uint8[2][4]"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"encryptedBallot","type":"bytes32"},{"name":"senderPubkey","type":"bytes32"}],"name":"submitBallot","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"ballotEncryptionPubkey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"ballotEncryptionSeckey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"endTime","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getEncSeckey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"}],"name":"voterToBallotID","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getBallotOptNumber","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"}],"name":"bannedAddresses","outputs":[{"name":"","type":"bool"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"uint256"}],"name":"associatedAddresses","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"swarmFundAddress","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"getEncPubkey","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"uint256"}],"name":"encryptedBallots","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"startTime","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_addr","type":"address"}],"name":"banAddress","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"owner","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"uint256"}],"name":"associatedPubkeys","outputs":[{"name":"","type":"bytes32"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_secKey","type":"bytes32"}],"name":"revealSeckey","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[{"name":"newEndTime","type":"uint256"}],"name":"setEndTime","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"testMode","outputs":[{"name":"","type":"bool"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[{"name":"_startTime","type":"uint256"},{"name":"_endTime","type":"uint256"},{"name":"_encPK","type":"bytes32"},{"name":"enableTesting","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"creator","type":"address"},{"indexed":false,"name":"start","type":"uint256"},{"indexed":false,"name":"end","type":"uint256"},{"indexed":false,"name":"encPubkey","type":"bytes32"}],"name":"CreatedBallot","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"voter","type":"address"},{"indexed":false,"name":"reason","type":"string"}],"name":"FailedVote","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"voter","type":"address"},{"indexed":false,"name":"ballot","type":"bytes32"},{"indexed":false,"name":"pubkey","type":"bytes32"}],"name":"SuccessfulVote","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"secretKey","type":"bytes32"}],"name":"SeckeyRevealed","type":"event"},{"anonymous":false,"inputs":[],"name":"TestingEnabled","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"error","type":"string"}],"name":"Error","type":"event"}];
const erc20Abi = [{"constant":true,"inputs":[],"name":"name","outputs":[{"name":"","type":"string"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_spender","type":"address"},{"name":"_amount","type":"uint256"}],"name":"approve","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"totalSupply","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_amount","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"decimals","outputs":[{"name":"","type":"uint8"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"owner","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"symbol","outputs":[{"name":"","type":"string"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_amount","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"_from","type":"address"},{"indexed":true,"name":"_to","type":"address"},{"indexed":false,"name":"_value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"_owner","type":"address"},{"indexed":true,"name":"_spender","type":"address"},{"indexed":false,"name":"_value","type":"uint256"}],"name":"Approval","type":"event"}];


var coinbase = "";
var accounts = [];


const legacyAddr = "0x2bb10945e9f0c9483022dc473ab4951bc2a77d0f".toLowerCase();


function convertAllToStdString(input) {
    if (input) {
        if (input.toFixed) {  // checks if it's a BigNumber
            return input.toFixed();
        }
        if (input.constructor === Array) {
            // do anything here?
        }
        return input.toString();
    } else {
        return input
    }
}


exports.setWeb3ProviderImpl = function(host, auth) {
    var userPass = {user: null, pass: null}
    if (auth !== "") {
        console.log("Using authenticated Web3 provider")
        var authArr = split(':', auth);
        userPass.user = authArr[0];
        userPass.pass = authArr[1];
        web3.setProvider(new Web3.providers.HttpProvider(host, 0, userPass.user, userPass.pass));
    } else {
        console.log("Using unauthenticated Web3 provider")
        web3.setProvider(new Web3.providers.HttpProvider(host));
    }
    console.log("Set web3 provider to:", host);
    web3.eth.getAccounts(function(err, acc) {
        accounts = acc;
        coinbase = accounts[0];
        web3.eth.defaultAccount = coinbase;  // needed to set this - thought it was auto?
    });
}


exports.getAccountImpl = function(left, right, n) {
    // console.info("Auditor getting account:", n);
    const acc = accounts[n];
    if (acc) {
        return right(acc)
    }
    return left("Account not found");
}


exports.getBlockNumberImpl = function() {
    return function(onErr, onSucc) {
        try {
            web3.eth.getBlockNumber(function(err, blockNum) {
                if (err) {
                    onErr(err.toString())
                } else {
                    onSucc(blockNum);
                }
            });
        } catch (err) {
            onErr(err.toString());
        }
    }
}


exports.makeSwmVotingContractImpl = function(just, nothing, addr) {
    if (!ethUtils.isValidAddress(addr)) {
        console.log("Invalid address provided for voting contract");
        return nothing;
    }

    try {
        const _abi = addr.toLowerCase() == legacyAddr ? legacyAbi : abi;
        const contract = web3.eth.contract(_abi).at(addr);
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
            return right(convertAllToStdString(ans));
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
    return exports.getBallotPropAsyncWBlockNumImpl(_prop, _args, null, contract);
}


exports.getBallotPropAsyncWBlockNumImpl = function(_prop, _args, blockNum, contract) {
    const prop = clone(_prop);
    const args = clone(_args);
    return function(onErr, onSucc) {
        args.push({gas: 999999});
        var ballotAsyncCB = function(err, res) {
            if (err) {
                console.log("getBallotPropAsync got error from callback:", err);
                onErr(err)
            }
            onSucc(convertAllToStdString(res));
        }
        if (blockNum) {
            args.push(blockNum);
        }
        args.push(ballotAsyncCB);
        try {
            contract[prop].apply(null, args);
        } catch (err) {
            console.log("getBallotPropAsync got error sending with args:", args, "\nERROR:\n", err);
            onErr(err);
        }
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
