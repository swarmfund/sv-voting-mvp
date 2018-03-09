require('./css/admin-ui.css');
const R = require('ramda');

const getFlags = require('./js/getFlags');
const {web3js, mmDetected, mmWeb3, web3Ports} = require('./js/web3Stuff');

window.addEventListener('load', function () {
    const flags = getFlags();
    flags.mmDetected = mmDetected;

    const Elm = require('./src/SecureVote/SPAs/DelegationUI/Main.elm');
    const app = Elm.SecureVote.SPAs.DelegationUI.Main.fullscreen(flags);
    console.log("Environment variables are: ", R.map(v => v.slice ? v.slice(0,80) : v, flags));
    web3Ports(web3js, {mmDetected, mmWeb3}, app, {dev: flags.dev});
});
