const fs = require('fs');

const loadDetails = () => {
    try {
        const abi = JSON.parse(fs.readFileSync('./_solDist/SwarmVotingMVP.abi').toString());
        const bin = fs.readFileSync('./_solDist/SwarmVotingMVP.bin').toString();
        return [abi, bin];
    } catch (err) {
        log("Failed to find contract details.\n")
        log("Recieved error:".red, err);
        log("Jsonified:".magenta, JSON.stringify(err));
        log("");
        process.exit(1);
    }
}

module.exports = loadDetails;