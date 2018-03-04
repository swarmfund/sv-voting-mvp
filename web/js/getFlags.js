const svLightBallotBoxABI = require('../../_solDist/SVLightBallotBox.abi.json');
const svLightIndexABI = require('../../_solDist/SVLightIndex.abi.json');
const delegationABI = require('../../_solDist/SVDelegation.abi.json');

const _DEV_ = process.env.DEV;
const DEV = (_DEV_ && _DEV_.toLowerCase() === "true") || false;

module.exports = function(){
    return {
        mainTitle: process.env.MAIN_TITLE,
        dev: DEV,
        democHash: process.env.DEMOC_HASH || "",
        indexABI: JSON.stringify(svLightIndexABI),
        ballotBoxABI: JSON.stringify(svLightBallotBoxABI),
        indexAddr: process.env.INDEX_ADDR,
    }
}
