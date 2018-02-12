import Web3 from "web3";
import ERC20ABI from "./ERC20ABI";
import SwmVotingMVPABIs from "./SwmVotingMVP.abi";
import abiDecoder from "abi-decoder";

import AuditWeb from "../../../../pureSrc/SecureVote/Democs/SwarmMVP/AuditWeb.purs";

import {create, env} from 'sanctuary';
const S = create({checkTypes: true, env});


// legacy contract w different getBallotOpts format
const legacyContractAddr = "0x2bb10945e9f0c9483022dc473ab4951bc2a77d0f";



const promiseCb = (resolve, reject, extra = []) => (err, val) => {
    console.log('promiseCb got:', {err, val, extra});
    if (err) {
        reject(err);
    } else {
        resolve([val, ...extra]);
    }
}


const web3Ports = (web3js, {mmDetected, mmWeb3}, app) => {
    if (mmDetected) {
        app.ports.gotMetamaskImpl.send(true);
    }

    const wrapper = (f) => {
        return (...args) => {
            try {
                f(...args);
            } catch (err) {
                console.log("Got error in function:", f);
                console.log(err.toString());
                implNotifyErr(err.toString());
            }
        }
    };


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


    app.ports.getInit.subscribe(wrapper((contractAddr) => {

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

        if (contractAddr == legacyContractAddr){
            getBallotOpts(implRecieveBallotOptsCBLegacy);
        } else {
            getBallotOpts(implRecieveBallotOptsCB);
        }

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


    const implRecieveBallotOptsCB = (err, ballotOpts) => {
        console.log('implRecieveBallotOptsCB got:', err, ballotOpts)
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
                response: ballotOpts
            })
        }
    };

    const implSendReadResp = ({success, errMsg, method, response}) => {
        app.ports.contractReadResponse.send({success, errMsg, method, response});
    };

    const implSendErc20Balance = wrapper((balance) => {
        const toRet = balance.toString(10);
        console.log('implSendErc20Balance got:', toRet);
        app.ports.implErc20Balance.send(toRet);

    });

    const implNotifyErr = (err) => {
        console.log("Got Error:", err);
        app.ports.gotWeb3Error.send(err)
    };


    // Help with error handling boilerplate
    const handleErrOr = (f) => (err, resp) => {
        if (err) {
            console.log('handleErrOr got err:', err);
            implNotifyErr(err)
        } else {
            f(resp);
        }
    };


    // Port subscriptions
    app.ports.setWeb3Provider.subscribe(wrapper((web3Provider) => {
        web3js.setProvider(new Web3.providers.HttpProvider(web3Provider));
        console.log("Web3 provider set to:", web3js.currentProvider);
    }));

    app.ports.getErc20Balance.subscribe(wrapper(({contractAddress, userAddress}) => {
        console.log("getErc20Balance got params", {contractAddress, userAddress});
        const tokenContract = Erc20Contract.at(contractAddress);
        tokenContract.balanceOf.call(userAddress, handleErrOr(implSendErc20Balance))
    }))


    app.ports.constructDataParam.subscribe(wrapper(({encBallot, voterPubkey, votingContractAddr}) => {
        console.log("constructDataParam got params:", {encBallot, voterPubkey, votingContractAddr});
        const voteC = SwmVotingContract.at(votingContractAddr);
        const data = voteC.submitBallot.getData("0x" + encBallot, "0x" + voterPubkey);
        app.ports.implDataParam.send(data);
        console.log("constructDataParam sent: ", data);
    }))


    app.ports.getEncryptionPublicKey.subscribe(wrapper(contractAddr => {
        const voteC = SwmVotingContract.at(contractAddr);
        voteC.getEncPubkey(handleErrOr(app.ports.gotEncPubkey.send));
    }))

    app.ports.performContractRead.subscribe(wrapper((successF, failF, contractAddr, methodName, args) => {
        const voteC = SwmVotingContract.at(contractAddr);
        try {
            const response = voteC[methodName](...args);
            app.ports.contractReadResponse.send(successF(response));
        } catch (err) {
            app.ports.contractReadResponse.send(failF(err.toString()));
        }
    }))

    app.ports.checkTxid.subscribe(wrapper((txid) => {
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


    const auditUpdateF = (statusUpdate) => {
        console.log(statusUpdate)
        app.ports.gotAuditMsgImpl.send(statusUpdate);
    }

    app.ports.getBallotResults.subscribe(wrapper((args) => {
        console.log("Calling AuditWeb with: ", args);
        try {
            const resp = AuditWeb.main(args)(auditUpdateF)();
        } catch (err) {
            console.error("AuditWeb.main threw error: ", err);
        }
    }));

    app.ports.castMetaMaskVoteImpl.subscribe(wrapper((tx) => {
        console.log("Sending tx to MetaMask:", tx);
        mmWeb3.eth.sendTransaction(tx, (err, ret) => {
            if (err) {
                console.error("MetaMask error: ", err);
                throw err;
            }
            console.log("MetaMask returned: ", err, ret);
            app.ports.metamaskTxidImpl.send(ret);
        });
    }))
};

export default web3Ports;
