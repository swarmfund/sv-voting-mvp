const svLightBallotBoxABI = require('../../_solDist/SVLightBallotBox.abi.json');
const svLightIndexABI = require('../../_solDist/SVLightIndex.abi.json');
const delegationABI = require('../../_solDist/SVDelegationV0101.abi.json');
const Url = require('domurl');

const _DEV_ = process.env.DEV;
const DEV = (_DEV_ && _DEV_.toLowerCase() === "true") || false;

const url = new Url;

console.log(process.env);

module.exports = function(){
    const flags = {
        mainTitle: process.env.MAIN_TITLE,
        dev: DEV,
        democHash: process.env.DEMOC_HASH || "",
        indexABI: JSON.stringify(svLightIndexABI),
        ballotBoxABI: JSON.stringify(svLightBallotBoxABI),
        indexAddr: process.env.INDEX_ADDR,
        delegationAddr: process.env.DELEGATION_ADDR,
        delegationABI: JSON.stringify(delegationABI),
        enableEarlyResults: url.query.enableEarlyResults === "true" || false,
    }
    return flags;
}
