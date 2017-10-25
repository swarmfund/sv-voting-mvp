const ethUtils = require('ethereumjs-util');



exports.checkAddressImpl = function(just, nothing, addr) {
    if (ethUtils.isValidAddress(addr)){
        return just(addr);
    }
    return nothing;
}
