document.addEventListener('DOMContentLoaded', () => {
    const elm = Elm.Main.fullscreen()

    if (storageAvailable('localStorage')) {
        console.log('localStorage available')
        elm.ports.save.subscribe((obj) => {
            console.log("elm.ports.save")
            for (const [key, value] of Object.entries(obj)) {
                console.log('saving ' + key)
                localStorage.setItem(key, JSON.stringify(value))
            }
        })

        elm.ports.load.subscribe((keys) => {
            console.log("elm.ports.load")
            console.log(keys)

            const items = {}

            for (const key of keys) {
                const value = localStorage.getItem(key)
                if (value !== null) {
                    console.log('found value for ' + key)
                    items[key] = JSON.parse(value)
                }
            }

            elm.ports.onLoad.send(items)
        })

    }
    else {
      // Too bad, no localStorage for us
    }



})

// https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API
function storageAvailable(type) {
    try {
        var storage = window[type],
            x = '__storage_test__';
        storage.setItem(x, x);
        storage.removeItem(x);
        return true;
    }
    catch(e) {
        return e instanceof DOMException && (
            // everything except Firefox
            e.code === 22 ||
            // Firefox
            e.code === 1014 ||
            // test name field too, because code might not be present
            // everything except Firefox
            e.name === 'QuotaExceededError' ||
            // Firefox
            e.name === 'NS_ERROR_DOM_QUOTA_REACHED') &&
            // acknowledge QuotaExceededError only if there's something already stored
            storage.length !== 0;
    }
}
