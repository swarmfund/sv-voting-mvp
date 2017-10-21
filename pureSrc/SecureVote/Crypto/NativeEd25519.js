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
}

function setNodeSodium() {
    verifyDetached = function(sig, msg, pk) {
        return sodium.api.crypto_sign_verify_detached(sig, msg, pk);
    }

    signDetached = sodium.api.crypto_sign_detached;
}


if (cSodium) {  // we are on a phone
    setCSodium();
} else if (sodium) {
    setNodeSodium();
} else if (verifyDetached === null) {
    jsNacl.instantiate(function(nacl) {
        verifyDetached = nacl.crypto_sign_verify_detached;
        signDetached = nacl.crypto_sign_detached;
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