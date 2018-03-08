import Web3 from "web3";
import ERC20ABI from "./ERC20ABI";
import abiDecoder from "abi-decoder";

const {implNotifyErrF, wrapIncomingF} = require('../../../js/portHelpers');

import {create, env} from 'sanctuary';
const S = create({checkTypes: true, env});
// const toPairs = require('ramda/src/toPairs');
const filter = require('ramda/src/filter');
const zip = require('ramda/src/zip');
const uniq = require('ramda/src/uniq');
const map = require('ramda/src/map');
const isNil = require('ramda/src/isNil');

const AsyncPar = require('async-parallel');
import {BigNumber} from 'bignumber.js';


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
        resolve([val, ...extra]);
    }
};


const logPrm = (msg, f) => (arg) => {
    f = f || (x => x);
    console.log(msg, f(arg));
    return arg;
};


const ethAddrEq = (a1, a2) => {
    return a1.toLowerCase() === a2.toLowerCase();
};


const convertBigNums = (args) => {
    const _args = Array.isArray(args) ? args : [args];
    return map(a => {
        if (!isNil(a) && (isNil(a.s) || isNil(a.e) || isNil(a.c))) {
            // then we do not have a bignum (note: better way to check?)
            return a;
        } else if (!isNil(a) && a.toString) {
            // then we have an object with fields s, e, and c
            return a.toString(10);
        }
        return a
    }, _args)
}


const web3Ports = (web3js, {mmDetected, mmWeb3}, app, {AuditWeb}) => {
    if (mmDetected) {
        app.ports.gotMetamaskImpl.send(true);
    }

    const wrapIncoming = wrapIncomingF(app);

    const implNotifyErr = implNotifyErrF(app);


    /* START DELEGATION SECTION */


    app.ports.setTokenDelegationImpl.subscribe(wrapIncoming(({delegationABI, contractAddr, delegateAddr, tokenContract}) => {
        const delegateABIObj = JSON.parse(delegationABI);
        const delegateContract = web3js.eth.contract(delegateABIObj).at(contractAddr);

        console.log(`Getting data for setTokenDelegation(${tokenContract}, ${delegateAddr})`);
        const payload = delegateContract.setTokenDelegation.getData(tokenContract, delegateAddr);

        app.ports.gotDelegatePayloadImpl.send(payload);
    }));


    app.ports.setGlobalDelegationImpl.subscribe(wrapIncoming(({delegationABI, contractAddr, delegateAddr}) => {
        const delegateABIObj = JSON.parse(delegationABI);
        const delegateContract = web3js.eth.contract(delegateABIObj).at(contractAddr);

        const payload = delegateContract.setGlobalDelegation.getData(delegateAddr);

        app.ports.gotDelegatePayloadImpl.send(payload);
    }));


    /* END DELEGATION SECTION */



    // "Global" constants
    const Erc20Contract = web3js.eth.contract(ERC20ABI);


    // scan the index to get all ballots
    app.ports.getDemocHashes.subscribe(wrapIncoming(({indexABI, indexAddr, ballotBoxABI, democHash}) => {
        const indexABIObj = JSON.parse(indexABI);
        const bbABIObj = JSON.parse(ballotBoxABI);
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

                    console.log("Getting starting block")
                    const vc = web3js.eth.contract(bbABIObj).at(votingContract);
                    vc.startingBlockAround(handleErrOr(sBlock => {
                        const startingBlockEst = sBlock.toNumber();
                        console.log(`(${votingContract}).startingBlockAround:`, startingBlockEst);
                        app.ports.ballotInfoExtra.send({bHash: specHash, startingBlockEst});
                    }))
                }));
            }, S.range(0, n));
        })
    }));


    // get an ERC20 contract's abbreviation
    app.ports.getErc20Abrv.subscribe(wrapIncoming(({bHash, erc20Addr}) => {
        const erc20 = web3js.eth.contract(ERC20ABI).at(erc20Addr);
        const sendAbrv = abrv => app.ports.gotErc20Abrv.send({bHash, erc20Addr, abrv});

        mkPromise(erc20.symbol)()
            .then(abrv => {
                sendAbrv(abrv);
            })
            .catch(e => {
                sendAbrv("ERC20");
                implNotifyErr("Unable to get ERC20 Abbreviation for " + erc20Addr + ". Error returned: " + JSON.stringify(e).toString());
            });
    }));


    app.ports.getTxInfoContractWrite.subscribe(wrapIncoming(params => {
        const {to, abi, method, args} = params;
        console.log("getTxInfoContractWrite got:", params);

        const c = web3js.eth.contract(JSON.parse(abi)).at(to);
        const data = c[method].getData(...args);

        console.log("getTxInfo got data", data);
        const toRet = JSON.stringify({to, data}, null, 4);
        console.log("Sending to gotTxInfo", toRet);
        app.ports.gotTxInfo.send(toRet);
    }));


    // app.ports.getInit.subscribe(wrapIncoming(({addr, oTitles}) => {
    //     const contractAddr = addr;
    //
    //     SwmVotingContract = genSwmVotingContract(contractAddr);
    //
    //     const miniAbi = isLegacy(contractAddr) ? SwmVotingMVPABIs.miniAbiLegacy : SwmVotingMVPABIs.miniAbi;
    //     app.ports.implInit.send({
    //         miniAbi: JSON.stringify(miniAbi)
    //     })
    //
    //     const voteC = SwmVotingContract.at(contractAddr);
    //
    //
    //     const getBallotOpts = (cb) => {
    //         try {
    //             voteC.getBallotOptions(cb);
    //         } catch (err) {
    //             cb(err, null);
    //         }
    //     }
    //
    //     getBallotOpts(isLegacy(contractAddr) ? implRecieveBallotOptsCBLegacy : implRecieveBallotOptsCB(oTitles));
    //
    //     try {
    //         const doErr = err => {
    //             implBallotPeriod(err, [0,0]);
    //         }
    //         voteC.startTime((err1, _startTime) => {
    //             if (err1) {
    //                 doErr(err1);
    //             }
    //             const startTime = _startTime.toNumber();
    //             voteC.endTime((err2, _endTime) => {
    //                 if (err2) {
    //                     doErr(err2);
    //                 }
    //                 const endTime = _endTime.toNumber();
    //                 implBallotPeriod(null, [startTime, endTime]);
    //             });
    //         });
    //     } catch (err) {
    //         implBallotPeriod(err, [0,0]);
    //     }
    // }));


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
        console.log("Web3 provider set to:", web3js.currentProvider);
    }));

    app.ports.getErc20Balance.subscribe(wrapIncoming(params => {
        let {contractAddress, userAddress, chainIndex, delegationABI, delegationAddr} = params;
        userAddress = userAddress.toLowerCase();
        const ci_ = parseInt(chainIndex) || chainIndex || "latest";
        console.log("getErc20Balance got params", params);
        const tokenContract = Erc20Contract.at(contractAddress);
        const delegationC = web3js.eth.contract(JSON.parse(delegationABI)).at(delegationAddr);

        let total = new BigNumber(0);
        const addToTotal = n => {
            total = total.plus(n);
            return n;
        };
        const getBalancePrm = mkPromise(tokenContract.balanceOf);
        const resolveDlgtionsPrm = mkPromise(delegationC.resolveDelegation);

        // promises
        const processDelegations = (([v, tC]) => {
            // get the delegation of a voter
            return resolveDlgtionsPrm(v, tC)
                .then(([_dId, _prevDId, _blkAt, _delegatee, _delegator, _tC]) => {
                    // ensure that *we* are the delegatee and also not the delegator to avoid
                    // double counting or other shenanigans
                    if (ethAddrEq(_delegatee, userAddress) && !ethAddrEq(_delegator, userAddress)) {
                        return getBalancePrm(v, ci_)
                    } else {
                        return new BigNumber(0);
                    }
                }).then(addToTotal)
                .then(logPrm(`\nFound balance for ${v} delegating ${userAddress} of`, n => n.toString(10)))
                .catch(e => {
                    console.log(`Err in resolveDelegationsPrm: ${e.message}`);
                    console.log(e);
            })
        });

        const findDlgtionBals = () => mkPromise(delegationC.findPossibleDelegatorsOf)(userAddress)
            .then(([voters, tokenCs]) => {
                console.log(`Processing delegations of ${userAddress}: voters(${voters})`);
                const delegatorPairs = zip(voters, tokenCs);
                // filter out delegations that aren't for this token and get unique pairs
                const processedVPs = uniq(filter(([v, tC]) => ethAddrEq(tC, contractAddress), delegatorPairs));
                return AsyncPar.map(processedVPs, processDelegations);
            });

        const findUserBal = () => getBalancePrm(userAddress, ci_)
            .then(addToTotal)
            .then(logPrm("Found user balance of: ", n => n.toString(10)));

        AsyncPar.invoke([findDlgtionBals, findUserBal])
            .then(() => implSendErc20Balance(total))
            .then(() => console.log(`Sent balance total: ${total.toString(10)}`));
    }));


    app.ports.constructDataParam.subscribe(wrapIncoming((params) => {
        params.abi = JSON.parse(params.abi);
        const {ballot, useEnc, voterPubkey, votingContractAddr, abi} = params;
        console.log("constructDataParam got params:", params);
        const voteC = web3js.eth.contract(abi).at(votingContractAddr);
        let data;
        if (useEnc) {
            data = voteC.submitBallotWithPk.getData("0x" + ballot, "0x" + voterPubkey);
        } else {
            data = voteC.submitBallotNoPk.getData("0x" + ballot);
        }
        app.ports.implDataParam.send(data);
        console.log("constructDataParam sent: ", data);
    }));


    app.ports.getEncryptionPublicKey.subscribe(wrapIncoming(contractAddr => {
        const voteC = SwmVotingContract.at(contractAddr);
        voteC.getEncPubkey(handleErrOr(app.ports.gotEncPubkey.send));
    }));

    app.ports.performContractRead.subscribe(wrapIncoming(({abi, addr, method, args}) => {
        console.log(`Reading ${addr}.${method}(${args})`);
        const c = web3js.eth.contract(JSON.parse(abi)).at(addr);
        mkPromise(c[method])(...args)
            .then(response => {
                const resp = convertBigNums(response);
                console.log(`Read ${addr}.${method}(${args}) w/ response ${resp}`);
                app.ports.contractReadResponse.send({success: true, errMsg: "", method, resp, addr});
            }).catch(err => {
                console.log(`Error reading ${addr}.${method}(${args}) => ${err.message}`);
                app.ports.contractReadResponse.send({success: false, errMsg: err.message, method, response: null, addr});
        })
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
                    const logs = abiDecoder.decodeLogs(getTxlogs);
                    console.log(logs);
                    logMsg = logs[0].events[0].value;
                } catch (err) {
                    console.log('checkTxid decoding error broke with: ', err.toString());
                }
                ret = {data: getTx.input, confirmed: getTx.blockNumber !== null, gas: getTxgasUsed || 0, logMsg};
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

                delete tx.gas;
            }).then(() => {
                mmWeb3.eth.sendTransaction(tx, (err, ret) => {
                    if (err) {
                        console.error("MetaMask error: ", err);
                        // if we have a bad from address strip it out and try again.
                        // if (tx.from !== "") {
                        //     tx.from = "";
                        //     sendMMTx(tx);
                        // } else {
                        //     implNotifyErr("Metamask Error! " + err.toString());
                        // }
                        implNotifyErr("Metamask Error! " + err.toString());
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
