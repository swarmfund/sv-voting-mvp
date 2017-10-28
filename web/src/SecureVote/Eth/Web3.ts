import Web3 = require("web3");
import ERC20ABI from "./ERC20ABI";
import SwmVotingMVPABIs from "./SwmVotingMVP.abi";

import {create, env} from 'sanctuary';
const S = create({checkTypes: true, env});


const web3Ports = (web3: Web3, app) => {

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
    }


    // "Global" constants
    const Erc20Contract = web3.eth.contract(ERC20ABI);
    const SwmVotingContract = web3.eth.contract(SwmVotingMVPABIs.fullAbi);


    app.ports.getInit.subscribe(wrapper((contractAddr) => {
        app.ports.implInit.send({
            miniAbi: JSON.stringify(SwmVotingMVPABIs.miniAbi)
        })

        const voteC = SwmVotingContract.at(contractAddr);
        try {
            voteC.getBallotOptions(implRecieveBallotOptsCB);
        } catch (err) {
            implRecieveBallotOptsCB(err, null);
        }
    }))


    // Implementation of port sends
    const implRecieveBallotOptsCB = (err, ballotOpts) => {
        console.log('implRecieveBallotOptsCB got:', err, ballotOpts)
        if (err) {
            console.log('implRecieveBallotOptsCB error got:', err);
            app.ports.contractReadResponse.send({
                success: false,
                errMsg: err.toString(),
                method: "getBallotOptions",
                response: []
            })
        } else {
            const bOpts = S.map(([a, b]) => ([a.toNumber(), b.toNumber()]), ballotOpts);
            console.log('ballot opts returning', bOpts);
            app.ports.contractReadResponse.send({
                success: true,
                errMsg: "",
                method: "getBallotOptions",
                response: bOpts
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
    const handleErrOr = <T>(f: ((T) => void)) => (err, resp: T) => {
        if (err) {
            console.log('handleErrOr got err:', err);
            implNotifyErr(err)
        } else {
            f(resp);
        }
    };


    // Port subscriptions
    app.ports.setWeb3Provider.subscribe(wrapper((web3Provider) => {
        web3.setProvider(new Web3.providers.HttpProvider(web3Provider));
        console.log("Web3 provider set to:", web3.currentProvider);
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
        web3.eth.getTransaction(txid, (err, resp) => {
            console.log('checkTxid response:', resp);
            if (err) {
                implNotifyErr(err.toString());
            } else {
                const ret = resp == null ? {data: "", confirmed: false} : {data: resp.input, confirmed: resp.blockNumber !== null};
                app.ports.gotTxidCheckStatus.send(ret);
            }
        });
    }));
};

export default web3Ports;

