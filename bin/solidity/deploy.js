const fs = require('fs');
const Web3 = require('web3');
const yargs = require('yargs');
const colors = require('colors');
const Confirm = require('prompt-confirm');
const R = require('ramda');


web3 = new Web3();


const log = (msg, offset = 0) => {
    if (offset > 0) {
        console.log(" ".repeat(offset - 1), msg)
    } else {
        console.log(msg)
    }
}


loadDetails = require('./loadContractDetails');



const ethHashCheck = (h, lenBytes = 32) => {
    try {
        return h.slice(0, 2) === "0x" && h.length === (lenBytes * 2 + 2);
    } catch (e) {
        console.error("Error!".red + " " + e.toString().yellow);
        console.log("exiting...");
        process.exit();
    }
};



const main = () => {
    const args_ = yargs.options({
        "contractName": {
            type: 'string',
            describe: "The contract name of an alternate contract to deploy, do not include '.sol'"
        }
    }).help(false).version(false).argv;

    const args = (args_.contractName) ? args_ : yargs.options({
        'testing': {
            demandOption: false,
            default: false,
            describe: 'enable testing functions in the ballot smart contract',
            type: 'boolean'
        },
        'useEncryption': {
            demandOption: true,
            default: false,
            describe: 'determine whether the smart contract will accept encrypted ballots or not.',
            type: 'boolean'
        },
        "deploy": {
            describe: '_automatically_ deploy the contract (no user interaction)',
            default: false,
            demandOption: false,
            type: 'boolean'
        },
        "startTime": {
            demandOption: false,
            default: 0,
            describe: "Start time in Seconds since Unix Epoch (UTC)",
            type: 'number'
        },
        "endTime": {
            demandOption: true,
            describe: "End time in Seconds since Unix Epoch (UTC)",
            type: 'number'
        },
        "unsafeSkipChecks": {
            describe: "This skips all confirmation checks. DRAGONS BE HERE",
            type: 'boolean'
        },
        "web3Provider": {
            describe: "URI for web3 provider - HTTP only",
            default: "http://localhost:8545",
            type: 'string'
        },
        "contractName": {
            describe: "The contract name of an alternate contract to deploy, do not include '.sol'",
            type: 'string'
        },
        "specHash": {
            describe: "The Keccak256 (eth-sha3) hash of the ballot spec",
            type: 'string',
            demandOption: true,
        }
    }).version(false).argv;

    if (!args.contractName && !ethHashCheck(args.specHash)) {
        log("Error:".bgRed.white + " Ballot Encryption Pubkey is not 32 bytes formatted in Ethereum Hex (should be 66 characters long total)")
        process.exit(1);
    }

    const contractName = args.contractName || "LittleBallotBox";
    const [abi, bin] = loadDetails(contractName);


    web3.setProvider(new Web3.providers.HttpProvider(args.web3Provider))

    log("\n\nSummary of Deployment:\n".cyan.bold)

    if (!args.contractName) {
        const startTime = new Date(args.startTime * 1000);
        const endTime = new Date(args.endTime * 1000);
        log("Start Time: " + startTime.toString().yellow, 2);
        log("End Time: " + endTime.toString().yellow, 2);
        log("Use Encryption: " + args.useEncryption.toString().yellow, 2);
        log("Ballot Specification Hash:", 2)
        log(args.specHash.yellow, 4)
    }
    log("Sending from: " + web3.eth.coinbase.yellow, 2);
    log("\nBe sure to " + "double and triple check".magenta + " these before you go live!\n")

    log(">>> THIS IS THE LAST OPPORTUNITY YOU HAVE TO CHANGE THEM <<<".bgYellow.black + "\n")

    const correctDetails = new Confirm("Are these details _all_ correct?");

    // MEAT OF SCRIPT IS HERE

    const deployF = () => {
        const contract = web3.eth.contract(abi);

        // set the contract deployment arguments
        const contractArgs = args.contractName ? [] : [args.startTime, args.endTime, args.useEncryption, args.testing, args.specHash];

        // organise our arguments for getting final bytecode
        const bytecodeArgs = R.append({data: "0x" + bin}, contractArgs);
        // get the final bytecode for gas estimation
        const bytecode = contract.new.getData(...bytecodeArgs);

        // create our params for sending
        const sendParams = {data: "0x" + bin, from: web3.eth.coinbase};

        // get arguments for the compiled data gas estimate
        const compiledDataArgs = R.append(sendParams, contractArgs);
        const compiledData = contract.new.getData(...compiledDataArgs);
        const compiledSendParams = R.merge(sendParams, {data: compiledData});
        console.log("Attempting to get gas estimate for ", compiledSendParams);
        const estGas = web3.eth.estimateGas({...compiledSendParams});
        console.log("Gas estimate: ", estGas);

        // add gas estimates with some headroom
        compiledSendParams.gas = Math.round(estGas * 1.5);
        sendParams.gas = Math.round(estGas * 1.5);

        if (args.deploy) {
            log("About to deploy...")
            log("NOTE:".yellow + " The cli will become unresponsive until the transaction confirms. Please be patient. \n\n")
            log("\nContract Deploying!\n".green);

            const deployCallback = (err, deployed) => {
                if (err) {
                    log("WARNING:".red + " Ran into an error while deploying contract:")
                    log(err);
                    log("\nStringified error: " + JSON.stringify(err));
                    process.exit(1);
                } else {
                    log("Tx Hash: " + deployed.transactionHash.green);
                    if (deployed.address) {
                        log("Contract Addr: " + deployed.address.green + "\n\n");
                        log("          >>> Job Done - Exiting <<<          ".bgGreen.black)
                        process.exit(0);
                    } else {
                        log("Awaiting a confirmation...\n".cyan);
                    }
                }
            };

            // organise our final arguments and deploy!
            const deploymentArgs = R.concat(contractArgs, [sendParams, deployCallback])
            const r = contract.new(...deploymentArgs)
        } else {
            log("Contract to deploy:\n".green.bold);
            log(JSON.stringify(compiledSendParams, null, 2))
            log("\n\n^^^ Contract parameters to deploy are above ^^^\n".green.bold)
        }
    }

    if (!args.unsafeSkipChecks) {
        correctDetails.run()
            .then(isCorrect => {
                if (!isCorrect) {
                    log("Exiting: details not correct.")
                    process.exit(0);
                } else {
                    deployF();
                }
            })
    } else {
        deployF();
    }
}


main();
