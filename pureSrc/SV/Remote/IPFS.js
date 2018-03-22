var ipfsAPI = require('ipfs-api');

exports.connectImpl = function(host, port, prot) {
    return ipfsAPI(host, port, {protocol: prot});
}
