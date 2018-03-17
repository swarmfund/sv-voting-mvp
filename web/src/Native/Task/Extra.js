// const _secure_vote$sv_light$Native_Task_Extra = function() {
//     const filter = require('ramda/src/filter');
//     const zip = require('ramda/src/zip');
//     const uniq = require('ramda/src/uniq');
//     const map = require('ramda/src/map');
//     const isNil = require('ramda/src/isNil');
//
//     const AsyncPar = require('async-parallel');
//
//     const scheduler = _elm_lang$core$Native_Scheduler;
//     const rSucc = scheduler.succeed;
//     const rFail = scheduler.fail;
//
//     // HELPER FUNCTIONS
//
//     const mkPromise = f => (...args) => {
//         return new Promise((resolve, reject) => {
//             f(...args, (err, resp) => {
//                 err ? reject(err) : resolve(resp);
//             })
//         })
//     };
//
//
//     const doTask = (t) => {
//         const oldCb = t
//         return t
//     };
//
//
//     // MAIN NATIVE FUNCTIONS FOR EXPORT
//
//     const parallel = (tasks) => scheduler.nativeBinding(cb => {
//         const _tasks = Array.isArray(tasks) ? tasks : [];
//         console.log(tasks);
//         AsyncPar.map(tasks, doTask)
//             .then(res => cb(rSucc(res)))
//             .catch(err => cb(rFail(err.message)));
//     });
//
//     const encodeTask = (t) => t;
//
//     return {parallel, encodeTask};
// }();
