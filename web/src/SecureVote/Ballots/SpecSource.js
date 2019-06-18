const ifpsAPI = require('ipfs-api');
const bs58 = require('bs58');
import axios from 'axios';
import {sha256} from 'js-sha256';

import {wrapIncomingF, implNotifyErrF} from '../../../js/portHelpers';


const specSourcePorts = (app, opts) => {
    const {ipfsHost, ipfsPort, ipfsProtocol, dev} = opts || {dev: false};
    const wrapIncoming = wrapIncomingF(app);
    const implNotifyErr = implNotifyErrF(app);


    const _ipfsHost = ipfsHost || "ipfs.infura.io";
    const _ipfsPort = ipfsPort || 443;
    const _ifpsProtocol = ipfsProtocol || "https";

    const ballotArchiveHttp = dev ? "https://archive.test.secure.vote/" : "https://archive.secure.vote/";
    console.log(`SpecSource using ${ballotArchiveHttp} as backup source`);


    let ipfs = ifpsAPI(_ipfsHost, _ipfsPort, {protocol: _ifpsProtocol});


    const tryGettingBallotFromS3 = hash => {
        return axios(ballotArchiveHttp + hash + ".json");
    };


    const getSpecSuccessF = (id, cid) => jsonDocStr => {
        const blockJson = JSON.parse(jsonDocStr);
        app.ports.gotSpecFromIpfs.send({id, cid, block: blockJson});
    };


    const doHashWPrefix = content => {
        return "0x" + sha256(content);
    };


    const getSpecFromIpfs = ({id, cidHex}) => {
        const getSpecSuccess = getSpecSuccessF(id, cidHex);
        const _cidBuffer = Buffer.from(cidHex, 'hex');
        const cid = bs58.encode(_cidBuffer);
        console.log("IPFS requesting cid:", cid, "with id", id, "and hex", cidHex);

        // give us ~~7.5s~~ 75ms to get the ballot from IPFS
        new Promise((resolve, reject) => {
            // ipfs.block.get(cid).then(resolve);
            setTimeout(reject, 75);
        }).then(block => {
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
                // console.error("ipfs.block.get errored:", err);
                tryGettingBallotFromS3(id)
                    .then(response => {
                        if (response.status === 200) {
                            // get raw response
                            const jsonDocStr = response.request.response;
                            console.log("S3 got jsonDoc for", id);
                            const genHash = doHashWPrefix(jsonDocStr);
                            if (genHash === id) {
                                getSpecSuccess(jsonDocStr);
                            } else {
                                console.error("Got bad generated hash: ", {genHash, expected: id});
                                throw Error("Unable to verify hash of obj from IPFS");
                            }
                        } else if (response.status === 404) {
                            console.error("Archive returned 404 for", id);
                            throw Error(`Archive unable to recover ballot: ${id}`);
                        } else {
                            console.error("archive returned status other than 200: ", response);
                            throw Error("Unable to get object from archive due to bad response from server");
                        }
                    })
                    .catch(err => {
                        console.error("Getting bSpec with bHash", id, "encountered fatal error:", err);
                        app.ports.gotFailedSpecFromIpfs.send({id, cid, err: err.message});
                        implNotifyErr(`Error while getting ballot ${err.message}. Ballot ID: ${id}`);
                    });
            })
    };


    app.ports.getSpecFromIpfs.subscribe(wrapIncoming(getSpecFromIpfs));
}


export default specSourcePorts;
