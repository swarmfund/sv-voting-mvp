require('./css/admin-ui.css');

const littleBallotBoxABI = require('../_solDist/LittleBallotBox.abi.json');
const littleGovIndexABI = require('../_solDist/LittleGovIndex.abi.json');

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

    const _DEV_ = process.env.DEV;

    const DEV = (_DEV_ && _DEV_.toLowerCase() === "true") || false;

    const Elm = require('./src/SecureVote/SPAs/AdminUI/Main.elm');
    const app = Elm.SecureVote.SPAs.AdminUI.Main.fullscreen({
        mainTitle: process.env.MAIN_TITLE,
        dev: DEV,
        democHash: process.env.DEMOC_HASH
    });
    console.log("Environment variables are: ", process.env.MAIN_TITLE, process.env.DEV, process.env.DEMOC_HASH);
    // web3Ports(web3js, {mmDetected, mmWeb3}, app);
    // curve25519Ports(app);

});
