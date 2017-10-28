import Web3 = require("web3");
import ERC20ABI from "./ERC20ABI";
import SwmVotingMVPABIs from "./SwmVotingMVP.abi";



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


    app.ports.getInit.subscribe(wrapper((meh: boolean) => {
        app.ports.implInit.send({
            miniAbi: JSON.stringify(SwmVotingMVPABIs.miniAbi)
        })
    }))


    // Implementation of port sends
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
        const pubkey = voteC.getEncPubkey();
        app.ports.gotEncPubkey.send(pubkey);
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
};

export default web3Ports;

