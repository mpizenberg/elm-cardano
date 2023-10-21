function initElmCardanoJs(app) {
    app.ports.toWallet.subscribe(async (value) => {
        console.log("Received value destined to wallet:", value)
        try {
            if (value.requestType == "cip30-discover") {
                const wallets = await concurrentHandleCip30Discover()
                app.ports.fromWallet.send({responseType: "cip30-discover", wallets})
            } else if (value.requestType == "cip30-enable") {
                const {descriptor, api, walletHandle} = await enableCip30Wallet(value.id, value.extensions)
                app.ports.fromWallet.send({responseType: "cip30-enable", descriptor, api, walletHandle})
            } else if (value.requestType == "cip30-api") {
                const apiResponse = await handleApiRequest(value)
                app.ports.fromWallet.send(apiResponse)
            }
        } catch (error) {
            console.log("Hum .........", error)
        }
        // TODO: do something with value and send message to wallet
        
    })

    async function concurrentHandleCip30Discover() {
        const wallets = []
        if ("cardano" in window) {
            const startTime = performance.now()
            const walletsPromises = Object.keys(window.cardano).map(walletDescriptor)
            const results = await Promise.allSettled(walletsPromises)
            results.forEach(result => { if (result.status === 'fulfilled') {
                wallets.push(result.value)
            }});
            const endTime = performance.now()
            console.log(`Cardano CIP30 wallets discovered in ${endTime - startTime} ms:`, wallets)
        } else {
            console.log("Well there isn't any Cardano wallet here ^^")
        }
        return wallets
    }

    async function walletDescriptor(walletId) {
        const startTime = performance.now()
        const wallet = window.cardano[walletId]
        for (const key of ["name", "icon", "apiVersion", "isEnabled"]) {
            if (!(key in wallet)) throw new Error("Unknown key: " + key)
        }
        const extensions = wallet.supportedExtensions || [];
        const descriptor = {
            id : walletId,
            name : wallet.name,
            icon : wallet.icon,
            apiVersion : wallet.apiVersion,
            supportedExtensions : extensions.map(({cip}) => cip), // extract CIP integer ids
            isEnabled : await wallet.isEnabled(),
        }
        const endTime = performance.now()
        console.log(`${walletId} discovered in ${endTime - startTime} ms:`)
        return descriptor
    }

    async function enableCip30Wallet(walletId, extensionsIds) {
        const extensions = extensionsIds.map((cipId) => ({cip: cipId}))
        if (walletId in window.cardano) {
            // TODO handle potential APIError
            const walletHandle = window.cardano[walletId]
            // const api = await walletHandle.enable({extensions}) // Eternl incorrect handle of CIP30 enable
            const api = await walletHandle.enable(extensions) // Eternl incorrect handle of CIP30 enable
            const descriptor = await walletDescriptor(walletId)
            return {descriptor, api, walletHandle}
        } else {
            // TODO: return error saying there is no wallet with this id
            throw new Error("Wallet ID does not correspond to any installed wallet: " + walletId)
        }
    }

    async function handleApiRequest({id, api, method, args}) {
        if (id in window.cardano) {
            // Replace "null" by "undefined" in the args array
            correctArgs = args.map(item => item === null ? undefined : item)
            const response = await api[method](...correctArgs)
            return {
                responseType: "cip30-api",
                walletId: id,
                method,
                response,
            }
        } else {
            // TODO: return error saying there is no wallet with this id
            throw new Error("Wallet ID does not correspond to any installed wallet: " + walletId)
        }
    }

    function exposed(params) {}
  
    return {
      exposed: exposed,
    }
}