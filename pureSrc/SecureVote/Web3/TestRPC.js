const TestRPC = require('ethereumjs-testrpc');


exports.testRpcImpl = function(port) {
    return function(error, success){
        const server = TestRPC.server();
        
        server.listen(port, function(err, blockchain) {
            if (err) {
                error(err)
            } else {
                success(server);
            }
        })
    }
}