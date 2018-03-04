const ifpsAPI = require('ipfs-api');
const bs58 = require('bs58');
import {sha256} from 'js-sha256';

import {wrapIncomingF, implNotifyErrF} from '../../../js/portHelpers';


const specSourcePorts = (app, opts) => {
    const {ipfsHost, ipfsPort, ipfsProtocol} = opts || {};
    const wrapIncoming = wrapIncomingF(app);
    const implNotifyErr = implNotifyErrF(app);


    const _ipfsHost = ipfsHost || "ipfs.infura.io";
    const _ipfsPort = ipfsPort || 443;
    const _ifpsProtocol = ipfsProtocol || "https";


    let ipfs = ifpsAPI(_ipfsHost, _ipfsPort, {protocol: _ifpsProtocol});


    const tryGettingBallotFromS3 = hash => {
        return new Promise((resolve, reject) => {
            reject('S3 not implemented')
        });
    };


    const getSpecSuccessF = (id, cid) => jsonDocStr => {
        const blockJson = JSON.parse(jsonDocStr);
        app.ports.gotSpecFromIpfs.send({id, cid, block: blockJson});
    }


    const doHashWPrefix = content => {
        return "0x" + sha256(content);
    }


    const getSpecFromIpfs = ({id, cidHex}) => {
        const getSpecSuccess = getSpecSuccessF(id, cidHex);
        const _cidBuffer = Buffer.from(cidHex, 'hex');
        const cid = bs58.encode(_cidBuffer);
        console.log("IPFS requesting cid:", cid, "with id", id, "and hex", cidHex);
        ipfs.block.get(cid)
            .then(block => {
                console.log("IPFS found cid", cid, "with block", block);
                const genHash = doHashWPrefix(block._data);
                if (genHash === id) {
                    getSpecSuccess(block._data.toString());
                } else {
                    console.error("Got bad generated hash: ", {genHash, expected: id});
                    throw Error("Unable to verify hash of obj from IPFS");
                }
            })
            .catch(err => {
                console.error("ipfs.block.get errored:", err);
                tryGettingBallotFromS3(id)
                    .then(jsonDocStr => {
                        console.log("S3 got jsonDoc: ", jsonDocStr);
                        const genHash = doHashWPrefix(jsonDocStr);
                        if (genHash === id) {
                            getSpecSuccess(jsonDocStr);
                        } else {
                            console.error("Got bad generated hash: ", {genHash, expected: id});
                            throw Error("Unable to verify hash of obj from IPFS");
                        }
                    })
                    .catch(err => {
                        console.log("Getting bSpec with bHash", id, "encountered fatal error:", err);
                        app.ports.gotFailedSpecFromIpfs.send({id, cid, err});
                    });
            })
    };


    app.ports.getSpecFromIpfs.subscribe(wrapIncoming(getSpecFromIpfs));
}


export default specSourcePorts;


//1fb0c7f3ab5d7069fea9745dbbc1c8037604984dc8ab27aa9b72e3422040d39c
//77359400
