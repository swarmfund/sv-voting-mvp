'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./src/SecureVote/SPAs/SwarmMVP/Main.elm');
var app = Elm.SecureVote.SPAs.SwarmMVP.Main.embed(document.getElementById('sv-fullscreen'));
