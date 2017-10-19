import elliptic = require('elliptic');


const curve25519Ports = (app) => {
    console.log(elliptic);
    const ec = new elliptic.ec('curve25519');

    app.ports.genKeyPair.subscribe((_) => {
        const key = ec.genKeyPair();
        const keyArray = key.priv.toArray();
        const keyHex = key.priv.toJSON();
        const toSend = {array: keyArray, hex: keyHex};
        app.ports.receiveKeyPair.send(toSend);
        console.log("getKeyPair sent via receiveKeyPair:", toSend);
    });
}


export default curve25519Ports;
