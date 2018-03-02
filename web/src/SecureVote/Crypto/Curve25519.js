const jsNacl = require('js-nacl');


const localStorageLogKey = "swmVotingKeyLog";


const doLog = (hexSk, hexPk, comment) => {
    try {
        const existingKeys = JSON.parse(localStorage.getItem(localStorageLogKey) || "[]");
        existingKeys.push({hexSk, hexPk, comment});
        localStorage.setItem(localStorageLogKey, JSON.stringify(existingKeys));
    } catch (err) {
        console.error("Got error while attempting to append voting key log: ", err);
    }
}


const curve25519Ports = (app) => {
    jsNacl.instantiate(nacl => {
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
            const keyPair = nacl.crypto_box_keypair();
            const hexSk = nacl.to_hex(keyPair.boxSk);
            const hexPk = nacl.to_hex(keyPair.boxPk);
            const toSend = {hexSk, hexPk};
            app.ports.receiveKeyPair.send(toSend);
            console.log("getKeyPair sent via receiveKeyPair:", toSend);
            doLog(hexSk, hexPk, "generated keys @ " + Date.now().toString());
        }));

        app.ports.encryptBytes.subscribe(catchErrors(({hexSk, hexRemotePk, bytesToSign}) => {
            console.log("encryptBytes got:", hexSk, hexRemotePk, bytesToSign)
            const msg = new Uint8Array(bytesToSign);
            const sk = nacl.from_hex(hexSk);
            const ourPk = nacl.crypto_box_keypair_from_raw_sk(sk).boxPk;
            const theirPk = nacl.from_hex(hexRemotePk);
            // we set the nonce to the first 24 bytes of the hash of our pubkey
            // this is okay as this is a once time use key
            // additionally, this means we don't have to publish the nonce 👍
            const nonce = nacl.crypto_hash_sha256(ourPk).slice(0,24);
            // note: leading zeros already trimmed - length will be 16 + msg.length
            const encrypted = nacl.crypto_box(msg, nonce, theirPk, sk);
            const encryptedHex = nacl.to_hex(encrypted);
            // const arrayToReturn = Array.prototype.slice.call(encrypted);
            app.ports.receiveEncryptedBytes.send(encryptedHex);
            console.log("encryptBytes sent via receiveEncryptedBytes:", encryptedHex);
            doLog(hexSk, hexRemotePk, "Signing " + nacl.to_hex(msg) + " with the secretKey to be opened by the public key. result: " + nacl.to_hex(encryptedHex));
        }));
    })

}


export default curve25519Ports;
