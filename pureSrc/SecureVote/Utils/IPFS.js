var bs58 = require('bs58');


exports.hexHashToSha256Bs58 = function(h) {
    return bs58.encode(Buffer.from("1220" + h, 'hex'));
};
