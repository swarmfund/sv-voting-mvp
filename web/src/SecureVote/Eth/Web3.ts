import Web3 = require("web3");
import ERC20ABI from "./ERC20ABI";
import BigNumber from "bignumber.js";

const web3Ports = (web3: Web3, app) => {
    // "Global" constants
    console.log("ERC20ABI is", ERC20ABI)
    const Erc20Contract = web3.eth.contract(ERC20ABI);


    // Implementation of port sends
    const implSendErc20Balance = (balance: BigNumber) => {
        const toRet = balance.toString(10);
        console.log('implSendErc20Balance got:', toRet);
        app.ports.implErc20Balance.send(toRet);
    };

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
    app.ports.setWeb3Provider.subscribe((web3Provider) => {
        web3.setProvider(new web3.providers.HttpProvider(web3Provider));
        console.log("Web3 provider set to:", web3.currentProvider);
    });

    app.ports.getErc20Balance.subscribe(({contractAddress, userAddress}) => {
        console.log("getErc20Balance got params", {contractAddress, userAddress});
        const tokenContract = Erc20Contract.at(contractAddress);
        tokenContract.balanceOf.call(userAddress, handleErrOr(implSendErc20Balance))
    })

};


export default web3Ports;

