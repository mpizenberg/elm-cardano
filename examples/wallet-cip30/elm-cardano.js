function initElmCardanoJs(app) {
    app.ports.toWallet.subscribe(async (value) => {
        console.log("Received value destined to wallet:", value)
        try {
            if (value.requestType == "cip30-discover") {
                const wallets = await concurrentHandleCip30Discover()
                app.ports.fromWallet.send({responseType: "cip30-discover", wallets})
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

    function exposed(params) {}
  
    return {
      exposed: exposed,
    }
}