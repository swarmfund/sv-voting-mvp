import jsNacl = require('js-nacl');


type SignBytesIncoming = {hexSk: string, hexRemotePk: string, bytesToSign: Array<number>};


const curve25519Ports = (app) => {
    jsNacl.instantiate(nacl => {
        const catchErrors = (f) => (...args) => {
            try {
                f(...args);
            } catch (err) {
                const _err = err.message ? JSON.stringify(err.message) : JSON.stringify(err);
                console.log('Caught curve25519 error:', _err)
                app.ports.receiveCurve25519Error.send(_err);
            }
        };

        app.ports.genKeyPair.subscribe(catchErrors((_) => {
            const keyPair = nacl.crypto_box_keypair();
            const toSend = {hexSk: nacl.to_hex(keyPair.boxSk), hexPk: nacl.to_hex(keyPair.boxPk)};
            app.ports.receiveKeyPair.send(toSend);
            console.log("getKeyPair sent via receiveKeyPair:", toSend);
        }));

        app.ports.encryptBytes.subscribe(catchErrors(({hexSk, hexRemotePk, bytesToSign}: SignBytesIncoming) => {
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
            const arrayToReturn = Array.prototype.slice.call(encrypted);
            app.ports.receiveEncryptedBytes.send(arrayToReturn);
            console.log("encryptBytes sent via receiveEncryptedBytes:", arrayToReturn);
        }));
    })

}


export default curve25519Ports;
