require('./css/admin-ui.css');
const map = require('ramda/src/map');

const getFlags = require('./js/getFlags');

const {initHashPorts} = require('./src/SecureVote/Crypto/Hashing');
const {web3js, mmDetected, mmWeb3, web3Ports} = require('./js/web3Stuff');

window.addEventListener('load', function () {
    const flags = getFlags();

    const Elm = require('./src/SecureVote/SPAs/AdminUI/Main.elm');
    const app = Elm.SecureVote.SPAs.AdminUI.Main.fullscreen(flags);
    console.log("Environment variables are: ", map(v => v.slice ? v.slice(0,80) : v, flags));
    console.log(web3Ports);
    web3Ports(web3js, {mmDetected, mmWeb3}, app, {dev: flags.dev});
    // curve25519Ports(app);
    initHashPorts(app);
});
