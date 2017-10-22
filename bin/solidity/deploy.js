const fs = require('fs');
const Web3 = require('web3');
const yargs = require('yargs');
const colors = require('colors');
const Confirm = require('prompt-confirm');
const R = require('ramda');


web3 = new Web3();


const log = (msg, offset=0) => {
    if (offset > 0) {
        console.log(" ".repeat(offset - 1), msg)
    } else {
        console.log(msg)
    }
}


loadDetails = require('./loadContractDetails');
const [abi, bin] = loadDetails();


const main = () => {
    const args = yargs.options({
      'testing': {
        demandOption: false,
        default: false,
        describe: 'enable testing functions in the ballot smart contract',
        type: 'boolean'
      },
      "deploy": {
        describe: '_automatically_ deploy the contract',
        type: 'boolean'
      },
      "startTime": {
          demandOption: true,
          describe: "Start time in Seconds since Unix Epoch (UTC)",
          type: 'number'
      },
      "endTime": {
        demandOption: true,
        describe: "End time in Seconds since Unix Epoch (UTC)",
        type: 'number'
      },
      "ballotEncPubkey": {
          demandOption: true,
          describe: "The curve25519 _public key_ selected for ballot encryption",
          type: 'string'
      },
      "unsafeSkipChecks": {
          describe: "This skips all confirmation checks. DRAGONS BE HERE",
          type: 'boolean'
      },
      "web3Provider": {
          describe: "URI for web3 provider - HTTP only",
          default: "http://localhost:8545",
          type: 'string'
      }
    }).version(false).argv;
    
    if (args.ballotEncPubkey.length != 66 || args.ballotEncPubkey.slice(0,2) != "0x") {
        log("Error:".bgRed.white + " Ballot Encryption Pubkey is not 64 bytes formatted in Ethereum Hex")
        process.exit(1);
    }

    web3.setProvider(new Web3.providers.HttpProvider(args.web3Provider))
    
    log("\n\nSummary of Deployment:\n".cyan.bold)

    const startTime = new Date(args.startTime * 1000);
    const endTime = new Date(args.endTime * 1000);
    log("Start Time: " + startTime.toString().yellow, 2);
    log("End Time: " + endTime.toString().yellow, 2);
    log("Ballot Encryption Pubkey:", 2)
    log(args.ballotEncPubkey.yellow, 4)
    log("Sending from: " + web3.eth.coinbase.yellow, 2);
    log("\nBe sure to " + "double and triple check".magenta + " these before you go live!\n")
    
    log(">>> THIS IS THE LAST OPPORTUNITY YOU HAVE TO CHANGE THEM <<<".bgYellow.black + "\n")

    const correctDetails = new Confirm("Are these details _all_ correct?");

    // MEAT OF SCRIPT IS HERE

    const deployF = () => {
        const contract = web3.eth.contract(abi);
        const bytecode = contract.new.getData(args.startTime, args.endTime, args.ballotEncPubkey, args.testing, {data: "0x" + bin});
        
        const sendParams = {data: "0x" + bin, from: web3.eth.coinbase};
        const compiledData = contract.new.getData(args.startTime, args.endTime, args.ballotEncPubkey, args.testing, sendParams)
        const compiledSendParams = R.merge(sendParams, {data: compiledData});
        const estGas = web3.eth.estimateGas(compiledSendParams);

        compiledSendParams.gas = Math.round(estGas * 1.5);
        sendParams.gas = Math.round(estGas * 1.5);

        if (args.deploy) {
            log("About to deploy...")
            log("NOTE:".yellow + " The cli will become unresponsive until the transaction confirms. Please be patient. \n\n")
            log("\nContract Deploying!\n".green);
            const r = contract.new(args.startTime, args.endTime, args.ballotEncPubkey, args.testing, sendParams, (err, deployed) => {
                if (err) {
                    log("WARNING:".red + " Ran into an error while deploying contract:")
                    log(err);
                    log("\nStringified error: " + JSON.stringify(err));
                    process.exit(1);
                } else {
                    log("Tx Hash: " + deployed.transactionHash.green);
                    if(deployed.address) {
                        log("Contract Addr: " + deployed.address.green + "\n\n");
                        log("          >>> Job Done - Exiting <<<          ".bgGreen.black)
                        process.exit(0);
                    } else {
                        log("Awaiting a confirmation...\n".cyan);
                    }
                }
            })
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