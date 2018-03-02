const littleBallotBoxABI = require('../../_solDist/LittleBallotBox.abi.json');
const littleGovIndexABI = require('../../_solDist/LittleGovIndex.abi.json');

const _DEV_ = process.env.DEV;
const DEV = (_DEV_ && _DEV_.toLowerCase() === "true") || false;

module.exports = function(){
    return {
        mainTitle: process.env.MAIN_TITLE,
        dev: DEV,
        democHash: process.env.DEMOC_HASH || "",
        indexABI: JSON.stringify(littleGovIndexABI),
        ballotBoxABI: JSON.stringify(littleBallotBoxABI),
        indexAddr: process.env.INDEX_ADDR,
    }
}
