'use strict';

// Require index.html so it gets copied to dist
require('./css/securevote-swarm.css');
require('./css/tachyons.min.css');
require('./css/vendor/material.amber-light_blue.min.css');
const Web3 = require('web3');

require('./js/birds');

const Elm = require('./src/SecureVote/SPAs/SwarmMVP/Main.elm');
const app = Elm.SecureVote.SPAs.SwarmMVP.Main.embed(document.getElementById('sv-fullscreen'));



