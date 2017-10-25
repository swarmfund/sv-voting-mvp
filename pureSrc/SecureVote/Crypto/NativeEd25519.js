"use strict"

try {
    var sodium = require('sodium');  // nodejs
} catch (e) {}
try {
    var cSodium = window.plugins.MiniSodium;  // cordova
} catch (e) {}
try {
    var jsNacl = require('js-nacl');  // fallback, pure js, slow!
} catch (e) {}


var verifyDetached = null;
var signDetached = null;
var genCurve25519Key = null;
var sha256 = null;


function setCSodium() {
    verifyDetached
        = function (signature, message, publicKey) {
            var success = false;
            var cb = function (err, value) {
                if (err || value === false || value === 0) {
                    success = false;
                } else {
                    success = true;
                }
            };

            cSodium.crypto_sign_verify_detached(signature, message, publicKey, cb);
            return success;
        };

    // toDo
    signDetached = cSodium.crypto_sign_detached;

    genCurve25519Key = function() {

    }

    sha256 = function(inputUI8A) {
        return Uint8Array.from(cSodium.Hash.sha256(inputUI8A));
    }
}

function setNodeSodium() {
    // verifyDetached = sodium.api.crypto_sign_verify_detached;
    // signDetached = sodium.api.crypto_sign_detached;
    sha256 = sodium.api.crypto_hash_sha256;
}


if (cSodium) {  // we are on a phone
    console.log("NativeEd25519: ")
    setCSodium();
} else if (sodium) {
    setNodeSodium();
} else if (verifyDetached === null) {
    jsNacl.instantiate(function(nacl) {
        verifyDetached = nacl.crypto_sign_verify_detached;
        signDetached = nacl.crypto_sign_detached;
        sha256 = nacl.crypto_hash_sha256;
    })
}

exports.verifyDetachedImpl
    = function (sig, msg, pk) {
        // console.log("verifyDetached:", sig, msg, pk);
        return verifyDetached(sig, msg, pk);
    };

exports.signDetachedImpl = function (msg, sk) {
    return signDetached(msg, sk);
}

exports.genCurve25519KeyImpl = function() {
    // return genCurve25519Key();
}

exports.sha256Impl = sha256;