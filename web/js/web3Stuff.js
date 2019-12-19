import Web3Legacy from 'web3';
// import Web3OnePointO from './vendor/web3-1.0.min';

import web3Ports from '../src/SecureVote/Eth/Web3';


let web3js;
let mmWeb3;
let mmDetected = false;
let mmAcct = "";

if (typeof web3 !== 'undefined' && typeof ethereum !== 'undefined') {
    console.info("Metamask detected...", web3.currentProvider);
    web3js = new Web3Legacy(web3.currentProvider);
    mmDetected = true;
    mmWeb3 = web3;
    mmAcct = web3.eth.accounts[0];
} else {
    web3js = new Web3Legacy();
    mmWeb3 = web3js;
}

// window.web3 = new Web3OnePointO(web3js.currentProvider);

export {
    web3js,
    mmWeb3,
    mmDetected,
    mmAcct,
    web3Ports
};
