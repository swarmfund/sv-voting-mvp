




const web3Ports = (web3, app) => {

    app.ports.setWeb3Provider.subscribe((web3Provider) => {
        web3.setProvider(new Web3.providers.HttpProvider(web3Provider));
        console.log("Web3 provider set to:", web3.currentProvider);
    });



};

export default web3Ports;

