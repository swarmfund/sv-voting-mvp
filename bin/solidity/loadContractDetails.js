const fs = require('fs');
const colors = require('colors');


const loadDetails = (contractName = "SwarmVotingMVP") => {
    try {
        // console.log("Running from", process.cwd())
        const solDist = "_solDist"
        var prefix = ""
        for (var i = 0; i < 7; i++) {
            if (fs.existsSync("./" + prefix + solDist)) {
                // console.log("Found _solDist at:", "./" + prefix + solDist);
                break;
            }
            prefix += "../"
        }
        var prePath = "./" + prefix + solDist;
        const abi = JSON.parse(fs.readFileSync(prePath + '/' + contractName + '.abi').toString());
        const bin = fs.readFileSync(prePath + '/' + contractName + '.bin').toString();
        return [abi, bin];
    } catch (err) {
        console.log("Failed to find contract details.\n")
        console.log("Current working dir is:", process.cwd())
        console.log("Recieved error:".red, err);
        console.log("Jsonified:".magenta, JSON.stringify(err));
        console.log("");
        process.exit(1);
    }
}

module.exports = loadDetails;