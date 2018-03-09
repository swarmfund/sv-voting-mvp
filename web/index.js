// Require index.html so it gets copied to dist
require('./css/securevote-swarm.css');
require('./css/vendor/tachyons.min.css');
require('./css/vendor/material.amber-light_blue.min.css');
const R = require('ramda');

// current build issue with netlify - ignore for now
// import AuditWeb from "../pureSrc/SecureVote/Democs/SwarmMVP/AuditWeb.purs";

const Decimal = require('decimal.js');
window.Decimal = Decimal;

import curve25519Ports from './src/SecureVote/Crypto/Curve25519';
import specSourcePorts from './src/SecureVote/Ballots/SpecSource';
import lsPorts from './src/SecureVote/LocalStorage';

const getFlags = require('./js/getFlags');
const {web3js, mmDetected, mmWeb3, mmAcct, web3Ports} = require('./js/web3Stuff');

window.addEventListener('load', function() {
    const flags = getFlags();
    flags.mmAcct = mmAcct;
    flags.mmDetected = mmDetected;
    console.log(flags);
    console.log("Environment variables are: ", R.map(function(v){ if(v){ return v.slice ? v.slice(0,80) : v} else { return v }}, flags));

    if (!flags.dev) {
        require('./js/vendor/birds.js');
    }

    document.title = flags.mainTitle + " - By SecureVote";

    document.getElementById("loading-screen").classList.add('slide-out');

    const Elm = require('./src/SecureVote/SPAs/SwarmMVP/Main.elm');
    const app = Elm.SecureVote.SPAs.SwarmMVP.Main.embed(document.getElementById('sv-fullscreen'), flags);
    web3Ports(web3js, {mmDetected, mmWeb3}, app, {AuditWeb: null, dev: flags.dev});
    curve25519Ports(app);
    specSourcePorts(app, {dev: flags.dev});
    lsPorts(app, {dev: flags.dev});
}, 100);
