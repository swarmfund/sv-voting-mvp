const wrapIncomingF = app => f => {
    return (...args) => {
        try {
            f(...args);
        } catch (err) {
            console.log("Got error in function:", f);
            console.log(err.toString());
            implNotifyErrF(app)(err.toString());
        }
    }
};


const implNotifyErrF = app => err => {
    console.log("Got Error:", err);
    app.ports.gotWeb3Error.send(err)
};


module.exports = {wrapIncomingF, implNotifyErrF};
