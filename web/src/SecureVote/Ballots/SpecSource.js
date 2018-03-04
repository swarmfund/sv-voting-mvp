const ifpsAPI = require('ipfs-api');
const bs58 = require('bs58');

import {wrapIncomingF, implNotifyErrF} from '../../../js/portHelpers';




const specSourcePorts = (app, opts) => {
    const {ipfsHost, ipfsPort, ipfsProtocol} = opts || {};
    const wrapIncoming = wrapIncomingF(app);
    const implNotifyErr = implNotifyErrF(app);


    const _ipfsHost = ipfsHost || "ipfs.infura.io";
    const _ipfsPort = ipfsPort || 443;
    const _ifpsProtocol = ipfsProtocol || "https";


    let ipfs = ifpsAPI(_ipfsHost, _ipfsPort, {protocol: _ifpsProtocol});


    const getSpecFromIpfs = ({id, cidHex}) => {
        const _cidBuffer = Buffer.from(cidHex, 'hex');
        const cid = bs58.encode(_cidBuffer);
        console.log("IPFS requesting cid:", cid, "with id", id, "and hex", cidHex);
        ipfs.block.get(cid)
            .then(block => {
                console.log("IPFS found cid", cid, "with block", block);
                const blockJson = JSON.parse(block._data.toString());
                app.ports.gotSpecFromIpfs.send({id, cid, block: blockJson});
            }).catch(err => {
                app.ports.gotFailedSpecFromIpfs.send({id, cid, err});
        })
    };


    app.ports.getSpecFromIpfs.subscribe(wrapIncoming(getSpecFromIpfs));
}


export default specSourcePorts;


//1fb0c7f3ab5d7069fea9745dbbc1c8037604984dc8ab27aa9b72e3422040d39c
//77359400
