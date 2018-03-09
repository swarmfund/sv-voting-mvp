


const initLsPorts = (app, {dev}) => {
    const setLS = ({key, value}) => {
        console.log("Setting LS:", key, value);
        const r = localStorage.setItem(key, value);
    }

    const getLS = k => {
        console.log("Getting LS:", k);
        const r = localStorage.getItem(k);
        if (r === null) {
            returnGetLS(false, k, `LS Not Found: ${k}`)
        } else {
            returnGetLS(true, k, r);
        }
    }

    const returnGetLS = (success, key, value) => {
        if (success) {
            app.ports.localStorageResp.send({key, value})
        } else {
            app.ports.localStorageFail.send({key, msg: value})
        }
    }

    app.ports.setLocalStorage.subscribe(setLS);
    app.ports.getLocalStorage.subscribe(getLS);
}


export default initLsPorts;
