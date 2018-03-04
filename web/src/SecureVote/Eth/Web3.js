import Web3 from "web3";
import ERC20ABI from "./ERC20ABI";
import SwmVotingMVPABIs from "./SwmVotingMVP.abi";
import abiDecoder from "abi-decoder";

const {implNotifyErrF, wrapIncomingF} = require('../../../js/portHelpers');

import {create, env} from 'sanctuary';
const S = create({checkTypes: true, env});
const toPairs = require('ramda/src/toPairs');


// legacy contract w different getBallotOpts format
const legacyContractAddr = "0x2bb10945e9f0c9483022dc473ab4951bc2a77d0f";


const mkPromise = f => (...args) => {
    return new Promise((resolve, reject) => {
        f(...args, (err, resp) => {
            err ? reject(err) : resolve(resp);
        })
    })
};


const promiseCb = (resolve, reject, extra = []) => (err, val) => {
    console.log('promiseCb got:', {err, val, extra});
    if (err) {
        reject(err);
    } else {
        resolve([val, ...extra
        ]);
    }
}



const web3Ports = (web3js, {mmDetected, mmWeb3}, app, {AuditWeb}) => {
    if (mmDetected) {
        app.ports.gotMetamaskImpl.send(true);
    }

    const wrapIncoming = wrapIncomingF(app);

    const implNotifyErr = implNotifyErrF(app);


    /* START DELEGATION SECTION */


    /* END DELEGATION SECTION */



    // "Global" constants
    const Erc20Contract = web3js.eth.contract(ERC20ABI);
    let SwmVotingContract = web3js.eth.contract(SwmVotingMVPABIs.fullAbi);


    const isLegacy = (addr) => {
        return addr.toLowerCase() === legacyContractAddr.toLowerCase()
    };


    const genSwmVotingContract = (addr) => {
        const abi = isLegacy(addr) ? SwmVotingMVPABIs.fullAbiLegacy : SwmVotingMVPABIs.fullAbi;
        abiDecoder.addABI(abi);

        return web3js.eth.contract(abi);
    };


    // scan the index to get all ballots
    app.ports.getDemocHashes.subscribe(wrapIncoming(({indexABI, indexAddr, democHash}) => {
        const indexABIObj = JSON.parse(indexABI);
        const index = web3js.eth.contract(indexABIObj).at(indexAddr);

        index.nBallots(democHash, (e, nBI) => {
            const n = nBI.toNumber();
            console.log("getDemocHashes got", n, "total ballots");
            app.ports.democNBallots.send({democHash, n});
            S.map(i => {
                index.getNthBallot(democHash, i, handleErrOr((info) => {
                    console.log("getNthBallot: ", info);
                    const [specHash, extraData, votingContract, startTs] = info;
                    const sendBack = {democHash, i, specHash, extraData, votingContract, startTs: startTs.toNumber()};
                    console.log("nthBallot: ", i, sendBack);
                    app.ports.gotBallotInfo.send(sendBack);
                }));
            }, S.range(0, n));
        })
    }));


    // get an ERC20 contract's abreviation
    app.ports.getErc20Abrv.subscribe(wrapIncoming(({bHash, erc20Addr}) => {
        const erc20 = web3js.eth.contract(ERC20ABI).at(erc20Addr);
        const sendAbrv = abrv => app.ports.gotErc20Abrv.send({bHash, erc20Addr, abrv});

        try {
            mkPromise(erc20.symbol)()
                .then(abrv => {
                    sendAbrv(abrv);
                })
                .catch(e => {throw e});
        } catch (e) {
            sendAbrv("ERC20");
            implNotifyErr("Unable to get ERC20 Abbreviation for " + addr + ". Error returned: " + JSON.stringify(e).toString());
        }
    }));


    app.ports.getInit.subscribe(wrapIncoming(({addr, oTitles}) => {
        const contractAddr = addr;

        SwmVotingContract = genSwmVotingContract(contractAddr);

        const miniAbi = isLegacy(contractAddr) ? SwmVotingMVPABIs.miniAbiLegacy : SwmVotingMVPABIs.miniAbi;
        app.ports.implInit.send({
            miniAbi: JSON.stringify(miniAbi)
        })

        const voteC = SwmVotingContract.at(contractAddr);


        const getBallotOpts = (cb) => {
            try {
                voteC.getBallotOptions(cb);
            } catch (err) {
                cb(err, null);
            }
        }

        getBallotOpts(isLegacy(contractAddr) ? implRecieveBallotOptsCBLegacy : implRecieveBallotOptsCB(oTitles));

        try {
            const doErr = err => {
                implBallotPeriod(err, [0,0]);
            }
            voteC.startTime((err1, _startTime) => {
                if (err1) {
                    doErr(err1);
                }
                const startTime = _startTime.toNumber();
                voteC.endTime((err2, _endTime) => {
                    if (err2) {
                        doErr(err2);
                    }
                    const endTime = _endTime.toNumber();
                    implBallotPeriod(null, [startTime, endTime]);
                });
            });
        } catch (err) {
            implBallotPeriod(err, [0,0]);
        }
    }));


    // Implementation of port sends
    const implBallotPeriod = (err, [startTime, endTime]) => {
        console.log('implBallotPeriod got', err, [startTime, endTime]);
        if (err) {
            app.ports.contractReadResponse.send({
                success: false,
                errMsg: err.toString(),
                method: "ballotPeriod",
                response: {}
            })
        } else {
            app.ports.contractReadResponse.send({
                success: true,
                errMsg: "",
                method: "ballotPeriod",
                response: {startTime, endTime}
            })
        }
    }


    const implRecieveBallotOptsCBLegacy = (err, ballotOpts) => {
        console.log('implRecieveBallotOptsCBLegacy got:', err, ballotOpts)
        if (err) {
            app.ports.contractReadResponse.send({
                success: false,
                errMsg: err.toString(),
                method: "getBallotOptionsLegacy",
                response: []
            })
        } else {
            const bOpts = S.map(([a, b]) => ([a.toNumber(), b.toNumber()]), ballotOpts);
            app.ports.contractReadResponse.send({
                success: true,
                errMsg: "",
                method: "getBallotOptionsLegacy",
                response: bOpts
            })
        }
    };


    const implRecieveBallotOptsCB = oTitles => (err, ballotOpts) => {
        const hashes_ = S.map(web3js.sha3, oTitles);
        const padding = new Array(5-hashes_.length);
        // hash of empty string
        padding.fill("0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");
        const hashes = S.concat(hashes_, padding);
        console.log('implRecieveBallotOptsCB got:', err, ballotOpts, "with titles", oTitles, "and calculated hashes", hashes);
        if (err) {
            app.ports.contractReadResponse.send({
                success: false,
                errMsg: err.toString(),
                method: "getBallotOptions",
                response: []
            })
        } else {
            app.ports.contractReadResponse.send({
                success: true,
                errMsg: "",
                method: "getBallotOptions",
                response: {isGood: S.equals(hashes, ballotOpts), hashes: ballotOpts}
            })
        }
    };

    const implSendReadResp = ({success, errMsg, method, response}) => {
        app.ports.contractReadResponse.send({success, errMsg, method, response});
    };

    const implSendErc20Balance = wrapIncoming((balance) => {
        const toRet = balance.toString(10);
        console.log('implSendErc20Balance got:', toRet);
        app.ports.implErc20Balance.send(toRet);

    });


    // Help with error handling boilerplate
    const handleErrOr = (f) => (err, resp) => {
        if (err) {
            console.log('handleErrOr got err:', err);
            implNotifyErr(err);
        } else {
            f(resp);
        }
    };


    // Port subscriptions
    app.ports.setWeb3Provider.subscribe(wrapIncoming((web3Provider) => {
        web3js.setProvider(new Web3.providers.HttpProvider(web3Provider));
        window.web3.setProvider(web3js.currentProvider);
        console.log("Web3 provider set to:", web3js.currentProvider);
    }));

    app.ports.getErc20Balance.subscribe(wrapIncoming(({contractAddress, userAddress}) => {
        console.log("getErc20Balance got params", {contractAddress, userAddress});
        const tokenContract = Erc20Contract.at(contractAddress);
        tokenContract.balanceOf.call(userAddress, handleErrOr(implSendErc20Balance))
    }))


    app.ports.constructDataParam.subscribe(wrapIncoming(({encBallot, voterPubkey, votingContractAddr}) => {
        console.log("constructDataParam got params:", {encBallot, voterPubkey, votingContractAddr});
        const voteC = SwmVotingContract.at(votingContractAddr);
        const data = voteC.submitBallot.getData("0x" + encBallot, "0x" + voterPubkey);
        app.ports.implDataParam.send(data);
        console.log("constructDataParam sent: ", data);
    }))


    app.ports.getEncryptionPublicKey.subscribe(wrapIncoming(contractAddr => {
        const voteC = SwmVotingContract.at(contractAddr);
        voteC.getEncPubkey(handleErrOr(app.ports.gotEncPubkey.send));
    }))

    app.ports.performContractRead.subscribe(wrapIncoming((successF, failF, contractAddr, methodName, args) => {
        const voteC = SwmVotingContract.at(contractAddr);
        try {
            const response = voteC[methodName](...args);
            app.ports.contractReadResponse.send(successF(response));
        } catch (err) {
            app.ports.contractReadResponse.send(failF(err.toString()));
        }
    }));

    // // this function takes an object and returns a list of values sorted by the keys.
    // const convertArgs = (args) => {
    //     const pairs = toPairs(args);
    //     const ordParis = sortBy(([k, v]) => parseInt(k), pairs);
    //     return map(([k, v]) => v, ordParis);
    // };

    app.ports.performContractWriteMM.subscribe(wrapIncoming(({abi, addr, method, args}) => {
        console.log("mm contract write:", {abi, addr, method, args});
        const c = mmWeb3.eth.contract(JSON.parse(abi)).at(addr);
        const data = c[method].getData(...args);
        const tx = {to: addr, value: 0, data};
        sendMMTx(tx);
    }));

    app.ports.checkTxid.subscribe(wrapIncoming((txid) => {
        const p = new Promise((resolve, reject) => {
            web3js.eth.getTransaction(txid, promiseCb(resolve, reject));
        })
        p.then(([getTx]) => {
            return new Promise((resolve, reject) => {
                web3js.eth.getTransactionReceipt(txid, promiseCb(resolve, reject, [getTx]));
            })
        }).then(([getTxR, getTx]) => {
            console.log("checkTxid got tx and txR of:", getTx, getTxR);
            let ret
            if (getTxR === null || getTx === null) {
                ret = {data: "", confirmed: false, gas: 0, logMsg: ""};
            } else {
                let logMsg = "";
                try {
                    const logs = abiDecoder.decodeLogs(getTxR.logs);
                    console.log(logs);
                    logMsg = logs[0].events[0].value;
                } catch (err) {
                    console.log('checkTxid decoding error broke with: ', err.toString());
                }
                ret = {data: getTx.input, confirmed: getTx.blockNumber !== null, gas: getTxR.gasUsed || 0, logMsg};
            }
            app.ports.gotTxidCheckStatus.send(ret);
        }).catch(err => {
            implNotifyErr(err.toString());
        });
    }));


    // AUDITOR STUFF

    // keep track of the current audit session, and if we start a second audit session stop giving us updates.
    let auditCounter = 0;

    const auditUpdateF = (i) => (statusUpdate) => {
        if (i === auditCounter) {
            console.log(statusUpdate)
            app.ports.gotAuditMsgImpl.send(statusUpdate);
        } else {
            console.log("Got out of date message for audit session:", i, ". Message:", statusUpdate);
        }
    }

    app.ports.getBallotResults.subscribe(wrapIncoming((args) => {
        auditCounter++;
        console.log("Calling AuditWeb session", auditCounter, "with:", args);
        try {
            const resp = AuditWeb.main(args)(auditUpdateF(auditCounter))();
        } catch (err) {
            console.error("AuditWeb.main threw error: ", err);
        }
    }));

    const sendMMTx = (tx) => {
        console.log("Sending tx to MetaMask:", tx);
        if (!mmDetected) {
            return implNotifyErr("Cannot send transaction: MetaMask was not detected.");
        }
        mkPromise(mmWeb3.eth.getAccounts)()
            .then(acc => {
                if (acc === []) {
                    throw Error("MetaMask is locked!");
                }

                console.log("MetaMask returned accounts: ", acc);

                if (!tx.gas) {
                    return mkPromise(mmWeb3.eth.estimateGas)(tx)
                        .then(gasEst => {
                            console.log("Gas estimated at ", gasEst);
                            tx.gas = gasEst;
                        })
                }
            }).then(() => {
                mmWeb3.eth.sendTransaction(tx, (err, ret) => {
                    if (err) {
                        console.error("MetaMask error: ", err);
                        // if we have a bad from address strip it out and try again.
                        if (tx.from !== "") {
                            tx.from = "";
                            sendMMTx(tx);
                        } else {
                            implNotifyErr("Metamask Error! " + err.toString());
                        }
                    } else {
                        console.log("MetaMask returned: ", err, ret);
                        app.ports.metamaskTxidImpl.send(ret);
                    }
                });
            })
            .catch(err => {
                return implNotifyErr("MetaMask error: " + err.toString());
            });
    };

    app.ports.castMetaMaskVoteImpl.subscribe(wrapIncoming(sendMMTx));
};

export default web3Ports;
