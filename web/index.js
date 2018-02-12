// Require index.html so it gets copied to dist
require('./css/securevote-swarm.css');
require('./css/vendor/tachyons.min.css');
require('./css/vendor/material.amber-light_blue.min.css');
import Web3 from 'web3';

import web3Ports from './src/SecureVote/Eth/Web3.js';
import curve25519Ports from './src/SecureVote/Crypto/Curve25519';

window.addEventListener('load', function() {
    let web3js;
    let mmDetected = false;

    if (typeof web3 !== 'undefined') {
        console.log("Metamask detected...", web3.currentProvider);
        web3js = new Web3(web3.currentProvider);
        mmDetected = true;
    } else {
        let web3;
        web3js = new Web3();
        web3 = web3js;
    }

    const _DEV_ = process.env.DEV;

    const DEV = (_DEV_ && _DEV_.toLowerCase() === "true") || false;
    if (!DEV) {
        require('./js/birds.js');
    }

    setTimeout(() => {
        const Elm = require('./src/SecureVote/SPAs/SwarmMVP/Main.elm');
        const app = Elm.SecureVote.SPAs.SwarmMVP.Main.embed(document.getElementById('sv-fullscreen'), {
            mainTitle: process.env.MAIN_TITLE,
            dev: DEV
        });
        console.log("Environment variables are: ", process.env.MAIN_TITLE, process.env.DEV);
        web3Ports(web3js, {mmDetected, mmWeb3: web3}, app);
        curve25519Ports(app);
    }, 400);

    document.getElementById("loading-screen").classList.add('slide-out');
});
