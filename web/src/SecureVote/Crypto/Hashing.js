const {sha256} = require('js-sha256');


const {wrapIncomingF, implNotifyErrF} = require('../../../js/portHelpers');


const initHashPorts = app => {
    const wrapIncoming = wrapIncomingF(app);

    const implNotifyErr = implNotifyErrF(app);

    app.ports.doHashSha256.subscribe(wrapIncoming(({ input, output, toHash }) => {
        const hexConv = i => Buffer.from(i, 'hex').toString();
        const _input = {
            'Hex': hexConv,
            'EthHex': i => hexConv(i.slice(2)),
            'String': i => i
        }[input](toHash);

        const _output = {
            'Hex': i => i,
            'EthHex': i => "0x" + i,
            'String': i => Buffer.from(i, 'string').toString()
        }[output](sha256(_input));

        app.ports.gotHash.send({output, alg: "sha256", result: _output});
    }));
}

export {initHashPorts};
