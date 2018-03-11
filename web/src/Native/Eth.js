

const filter = require('ramda/src/filter');
const zip = require('ramda/src/zip');
const uniq = require('ramda/src/uniq');
const map = require('ramda/src/map');
const isNil = require('ramda/src/isNil');

const _secure_vote$sv_light$Native_Eth = function() {
    let web3js;

    const scheduler = _elm_lang$core$Native_Scheduler;
    const rSucc = scheduler.succeed;
    const rFail = scheduler.fail;

    // HELPER FUNCTIONS

    const convertBigNums = (args) => {
        const _args = Array.isArray(args) ? args : [args];
        return map(a => {
            if (!isNil(a) && (isNil(a.s) || isNil(a.e) || isNil(a.c))) {
                // then we do not have a bignum (note: better way to check?)
                return a;
            } else if (!isNil(a) && a.toString) {
                // then we have an object with fields s, e, and c
                return a.toString(10);
            }
            return a
        }, _args)
    }

    const mkPromise = f => (...args) => {
        return new Promise((resolve, reject) => {
            f(...args, (err, resp) => {
                err ? reject(err) : resolve(resp);
            })
        })
    };


    // MAIN NATIVE FUNCTIONS FOR EXPORT


    const init = (web3_) => scheduler.nativeBinding(cb => {
        web3js = web3_;
        console.log("Set scoped Web3 to new obj", web3js);
        cb(scheduler.succeed(null));
    });


    const readContract = ({abi, addr, method, args}) => {
        return scheduler.nativeBinding(cb => {
            try {
                addr = addr.toLowerCase();
                console.log(`Task: Reading ${addr}.${method}(${args})`);
                const c = web3js.eth.contract(JSON.parse(abi)).at(addr);
                console.log(`Got contract: ${c}`)
                mkPromise(c[method])(...args)
                    .then(response => {
                        const resp = convertBigNums(response);
                        console.log(`Read ${addr}.${method}(${args}) w/ response ${resp}`);
                        return cb(rSucc({method, resp, addr}));
                    }).catch(err => {
                        console.log(`Error reading ${addr}.${method}(${args}) => ${err.message}`);
                        return cb(rFail(err.message));
                })
            } catch (e) {
                console.log('ReadContract caught an error: ', e);
                return cb(scheduler.fail(e.message));
            }
        });
    };

    console.log(scheduler)

    return {init, readContract};
}();
