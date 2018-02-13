// import jsNacl = require('js-nacl');
import nacl = require('tweetnacl');
import sha256 from 'fast-sha256';

// https://github.com/tonyg/js-nacl/blob/master/lib/nacl_factory.js -- MIT License
function to_hex(bs) {
    var encoded = [];
    for (var i = 0; i < bs.length; i++) {
        encoded.push("0123456789abcdef"[(bs[i] >> 4) & 15]);
        encoded.push("0123456789abcdef"[bs[i] & 15]);
    }
    return encoded.join('');
}

function from_hex(s) {
    var result = new Uint8Array(s.length / 2);
    for (var i = 0; i < s.length / 2; i++) {
        result[i] = parseInt(s.substr(2 * i, 2), 16);
    }
    return result;
}



type SignBytesIncoming = {hexSk: string, hexRemotePk: string, bytesToSign: Array<number>};
type KeyLog = {hexSk: string, hexPk: string, comment: string};


const localStorageLogKey = "swmVotingKeyLog";


const doLog = (hexSk, hexPk, comment) => {
    try {
        const existingKeys: Array<KeyLog> = JSON.parse(localStorage.getItem(localStorageLogKey) || "[]");
        existingKeys.push({hexSk, hexPk, comment});
        localStorage.setItem(localStorageLogKey, JSON.stringify(existingKeys));
    } catch (err) {
        console.error("Got error while attempting to append voting key log: ", err);
    }
}


const curve25519Ports = (app) => {
    const catchErrors = (f) => (...args) => {
        try {
            f(...args);
        } catch (err) {
            const _err = err.message ? err.message.toString() : err.toString();
            console.log('Caught curve25519 error:', _err)
            app.ports.receiveCurve25519Error.send(_err);
        }
    };

    app.ports.genKeyPair.subscribe(catchErrors((_) => {
        const keyPair = nacl.box.keyPair()
        const hexSk = to_hex(keyPair.publicKey);
        const hexPk = to_hex(keyPair.secretKey);
        const toSend = {hexSk, hexPk};
        app.ports.receiveKeyPair.send(toSend);
        console.log("getKeyPair sent via receiveKeyPair:", toSend);
        doLog(hexSk, hexPk, "generated keys @ " + Date.now().toString());
    }));

    app.ports.encryptBytes.subscribe(catchErrors(({hexSk, hexRemotePk, bytesToSign}: SignBytesIncoming) => {
        console.log("encryptBytes got:", hexSk, hexRemotePk, bytesToSign)
        const msg = new Uint8Array(bytesToSign);
        const sk = from_hex(hexSk);
        const ourPk = nacl.box.keyPair.fromSecretKey(sk).publicKey;
        const theirPk = from_hex(hexRemotePk);
        // we set the nonce to the first 24 bytes of the hash of our pubkey
        // this is okay as this is a once time use key
        // additionally, this means we don't have to publish the nonce 👍
        const nonce = sha256(ourPk).slice(0,24);
        // note: leading zeros already trimmed - length will be 16 + msg.length
        const encrypted = nacl.box(msg, nonce, theirPk, sk);
        const encryptedHex = to_hex(encrypted);
        // const arrayToReturn = Array.prototype.slice.call(encrypted);
        app.ports.receiveEncryptedBytes.send(encryptedHex);
        console.log("encryptBytes sent via receiveEncryptedBytes:", encryptedHex);
        doLog(hexSk, hexRemotePk, "Signing " + to_hex(msg) + " with the secretKey to be opened by the public key. result: " + to_hex(encryptedHex));
    }));

}


export default curve25519Ports;
