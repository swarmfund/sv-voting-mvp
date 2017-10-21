"use strict"

const Web3 = require('web3');
const ethUtils = require('ethereumjs-util');
const votingABI = require('../contract/build/contracts/SwarmVotingMVP.json').abi;


const web3 = new Web3();


exports.setWeb3ProviderImpl = function(host) {
    web3.setProvider(new Web3.providers.HttpProvider(host));
    console.log("Set web3 provider to", host);
}


exports.makeSwmVotingContractImpl = function(just, nothing, addr) {
    console.log("makeSwmVotingContract called with address:", addr);
    if (!ethUtils.isValidAddress(addr)) {
        console.log("Invalid address provided for voting contract");
        return nothing;
    }

    try {
        const contract = web3.eth.contract(votingABI).at(addr);
        return just(contract);
    } catch (err) {
        console.log("WARNING: Got error when creating Web3 contract instance:", err);
        return nothing;
    }
    return nothing;
}


exports.getBallotSKImpl = function(just, nothing, contract) {
    console.log("getBallotSKImpl got contract at", contract.address)
    const encPK = contract.getEncPubkey(0);
    const nVotes = contract.nVotes(1);
    const owner = contract.owner(1);
    console.log(encPK, nVotes, owner);
    return nothing;
}