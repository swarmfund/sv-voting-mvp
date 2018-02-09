const Decimal = require("decimal.js");

exports.toFixed = function(ds) {
    return function(x) {
        return x.toFixed(ds);
    }
}
