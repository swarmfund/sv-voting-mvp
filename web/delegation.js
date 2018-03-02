require('./css/admin-ui.css');
const R = require('ramda');

const getFlags = require('./getFlags');
import Web3Legacy from 'web3';
// import Web3OnePointO from './js/vendor/web3-1.0.min';
import web3Ports from './src/SecureVote/Eth/Web3.js';

let web3;
let web3js;
let mmWeb3;
let mmDetected = false;

window.addEventListener('load', function () {

    if (typeof web3 !== 'undefined') {
        console.log("Metamask detected...", web3.currentProvider);
        web3js = new Web3Legacy(web3.currentProvider);
        mmDetected = true;
        mmWeb3 = web3;
    } else {
        web3js = new Web3Legacy();
        web3 = web3js;
        mmWeb3 = web3js;
    }

    window.web3 = web3;

    const flags = getFlags();

    const Elm = require('./src/SecureVote/SPAs/DelegationUI/Main.elm');
    const app = Elm.SecureVote.SPAs.DelegationUI.Main.fullscreen(flags);
    console.log("Environment variables are: ", R.map(v => v.slice ? v.slice(0,80) : v, flags));
    web3Ports(web3js, {mmDetected, mmWeb3}, app, {});
    // curve25519Ports(app);

});
