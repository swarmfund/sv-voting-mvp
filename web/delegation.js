require('./css/admin-ui.css');
const R = require('ramda');

const getFlags = require('./getFlags');

let web3;
let web3js;
let mmWeb3;
let mmDetected = false;

window.addEventListener('load', function () {

    if (typeof web3 !== 'undefined') {
        console.log("Metamask detected...", web3.currentProvider);
        web3js = new Web3(web3.currentProvider);
        mmDetected = true;
        mmWeb3 = web3;
    } else {
        web3js = new Web3();
        web3 = web3js;
        mmWeb3 = web3js;
    }

    window.web3 = web3;

    flags = getFlags();

    const Elm = require('./src/SecureVote/SPAs/DelegationUI/Main.elm');
    const app = Elm.SecureVote.SPAs.DelegationUI.Main.fullscreen(flags);
    console.log("Environment variables are: ", R.map(v => v.slice ? v.slice(0,80) : v, flags));
    // web3Ports(web3js, {mmDetected, mmWeb3}, app);
    // curve25519Ports(app);

});
