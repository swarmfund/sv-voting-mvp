// Require index.html so it gets copied to dist
require('./css/securevote-swarm.css');
require('./css/vendor/tachyons.min.css');
require('./css/vendor/material.amber-light_blue.min.css');
import Web3Legacy from 'web3';
import Web3OnePointO from './js/vendor/web3-1.0.min';
const R = require('ramda');

import web3Ports from './src/SecureVote/Eth/Web3.js';
import curve25519Ports from './src/SecureVote/Crypto/Curve25519';

const getFlags = require('./getFlags');

window.addEventListener('load', function() {
    let web3js;
    let mmWeb3;
    let mmDetected = false;

    if (typeof web3 !== 'undefined') {
        console.log("Metamask detected...", web3.currentProvider);
        web3js = new Web3Legacy(web3.currentProvider);
        mmDetected = true;
        mmWeb3 = web3;
    } else {
        web3js = new Web3Legacy();
        mmWeb3 = web3js;
    }

    window.web3 = new Web3OnePointO(web3js.currentProvider);

    const flags = getFlags();

    if (!flags.dev) {
        require('./js/birds.js');
    }

    document.title = flags.mainTitle + " - By SecureVote";

    document.getElementById("loading-screen").classList.add('slide-out');

    const Elm = require('./src/SecureVote/SPAs/SwarmMVP/Main.elm');
    const app = Elm.SecureVote.SPAs.SwarmMVP.Main.embed(document.getElementById('sv-fullscreen'), flags);
    console.log("Environment variables are: ", R.map(v => v.slice ? v.slice(0,80) : v, flags));
    web3Ports(web3js, {mmDetected, mmWeb3}, app);
    curve25519Ports(app);
}, 100);
