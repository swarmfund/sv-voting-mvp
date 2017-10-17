'use strict';

// Require index.html so it gets copied to dist
require('./css/securevote-swarm.css');
require('./css/tachyons.min.css');
require('./css/vendor/material.amber-light_blue.min.css');
const Web3 = require('web3');
const web3 = new Web3();

require('./js/birds');

import web3Ports from './src/SecureVote/Eth/Web3.js';

const Elm = require('./src/SecureVote/SPAs/SwarmMVP/Main.elm');
const app = Elm.SecureVote.SPAs.SwarmMVP.Main.embed(document.getElementById('sv-fullscreen'));

web3Ports(web3, app);
