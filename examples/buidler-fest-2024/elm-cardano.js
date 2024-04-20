function initElmCardanoJs(app) {

    // Navigation ##############################################################

    // Inform app of browser navigation (the BACK and FORWARD buttons)
    window.addEventListener('popstate', function () {
        app.ports.onUrlChange.send(location.href);
    });

    // Change the URL upon request, inform app of the change.
    app.ports.pushUrl.subscribe(function (url) {
        history.pushState({}, '', url);
        app.ports.onUrlChange.send(location.href);
    });

    // Wallet CIP30 ############################################################

    app.ports.toWallet.subscribe(async (value) => {
        console.log("Received value destined to wallet:", value)
        try {
            if (value.requestType == "cip30-discover") {
                const wallets = await concurrentHandleCip30Discover()
                app.ports.fromWallet.send({ responseType: "cip30-discover", wallets })
            } else if (value.requestType == "cip30-enable") {
                const { descriptor, api, walletHandle } = await enableCip30Wallet(value.id, value.extensions)
                app.ports.fromWallet.send({ responseType: "cip30-enable", descriptor, api, walletHandle })
            } else if (value.requestType == "cip30-api") {
                const apiResponse = await handleApiRequest(value)
                app.ports.fromWallet.send(apiResponse)
            }
        } catch (error) {
            app.ports.fromWallet.send({ responseType: "cip30-error", error: error })
        }
    })

    async function concurrentHandleCip30Discover() {
        const wallets = []
        if ("cardano" in window) {
            const startTime = performance.now()
            const walletsPromises = Object.keys(window.cardano).map(walletDescriptor)
            const results = await Promise.allSettled(walletsPromises)
            results.forEach(result => {
                if (result.status === 'fulfilled') {
                    wallets.push(result.value)
                }
            });
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
            if (!(key in wallet)) throw new Error(`Missing expected key in wallet '${walletId}': ${key}`)
        }
        const descriptor = {
            id: walletId,
            name: wallet.name,
            icon: wallet.icon,
            apiVersion: wallet.apiVersion,
            supportedExtensions: wallet.supportedExtensions || [],
            isEnabled: await wallet.isEnabled(),
        }
        const endTime = performance.now()
        console.log(`${walletId} discovered in ${endTime - startTime} ms:`)
        return descriptor
    }

    async function enableCip30Wallet(walletId, extensionsIds) {
        const extensions = extensionsIds.map((cipId) => ({ cip: cipId }))
        if (walletId in window.cardano) {
            const walletHandle = window.cardano[walletId]
            // const api = await walletHandle.enable({extensions}) // Eternl incorrect handle of CIP30 enable
            const api = await walletHandle.enable(extensions) // Eternl incorrect handle of CIP30 enable
            const descriptor = await walletDescriptor(walletId)
            return { descriptor, api, walletHandle }
        } else {
            throw new Error("Wallet ID does not correspond to any installed wallet: " + walletId)
        }
    }

    async function handleApiRequest({ id, api, method, args }) {
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
            throw new Error("Wallet ID does not correspond to any installed wallet: " + walletId)
        }
    }

    // OGMIOS ##################################################################

    app.ports.toOgmios.subscribe(async (value) => {
        // console.log("Received value destined to ogmios:", value)
        try {
            if (value.requestType == "ogmios-connect") {
                const websocket = await connectToOgmios(value.websocketAddress, value.connectionId)
                app.ports.fromOgmios.send({ responseType: "ogmios-connect", ws: websocket, connectionId: value.connectionId })
            } else if (value.requestType == "ogmios-disconnect") {
                value.ws.close()
            } else if (value.requestType == "ogmios-api") {
                handleApiRequest(value)
            }
        } catch (error) {
            app.ports.fromOgmios.send({ responseType: "ogmios-error", error: error })
        }
    })

    async function connectToOgmios(address, connectionId) {
        const client = new WebSocket(address)
        // Listen for messages
        client.addEventListener("message", (event) => {
            // console.log("Message from ogmios (", typeof event.data, "):", event.data)
            // Handle potential big integers
            const preprocessedData = bigIntToStringPreProcess(event.data)
            const parsedMessage = JSON.parse(preprocessedData)
            app.ports.fromOgmios.send({ responseType: "ogmios-message", connectionId, message: parsedMessage })
        });
        // Listen for possible errors
        client.addEventListener("error", (event) => {
            console.log("WebSocket error: ", event)
            app.ports.fromOgmios.send({ responseType: "ogmios-error", connectionId, error: event })
        });
        // Listen for closed connection
        client.addEventListener("close", (event) => {
            console.log("The connection has been closed successfully.")
            app.ports.fromOgmios.send({ responseType: "ogmios-disconnect", connectionId })
        });
        await waitForOpenSocket(client)
        return client
    }

    // Wait for the connection with Ogmios to be open
    async function waitForOpenSocket(socket) {
        return new Promise((resolve) => {
            if (socket.readyState !== socket.OPEN) {
                socket.addEventListener("open", (_) => {
                    resolve();
                })
            } else {
                resolve();
            }
        });
    }

    // Send the API remote call from Elm to Ogmios
    async function handleApiRequest({ ws, request }) {
        // Convert elm big integers back to JSON integers
        const jsonRequest = JSON.stringify(request, null, "") // must be compact
        ws.send(replaceBigIntObject(jsonRequest))
    }

    // Helper function to pre-process JSON strings.
    // This converts unsafe integers that would have a lossy conversion to JS number,
    // into a string that can be parsed losslessly as a string.
    // Kudos to Simon Lydell for this magic regex.
    // The regex matches every instance of either a string, or a number.
    // It should support all shapes of numbers like decimals, scientific notation etc.
    //
    // An alternative to this regex patching would be to parse the JSON string directly in Elm
    // with something like https://github.com/zwilias/elm-json-in-elm/tree/master
    //
    // Another alternative for doing this directly in Elm would be using allenap/elm-json-decode-broken
    // https://package.elm-lang.org/packages/allenap/elm-json-decode-broken/latest/Json-Decode-Broken#parseWith
    const stringOrNumber = /("(?:[^\\"]|\\.)*")|-?(?:[1-9]\d*|0)(?:\.\d+)?(?:[eE][+-]?\d+)?/g;
    const onlySignedDigits = /^-?\d+$/;
    function bigIntToStringPreProcess(jsonString) {
        return jsonString.replace(stringOrNumber, (match, stringLiteral) => {
            if (stringLiteral != undefined) {
                // match is a string
                return stringLiteral;
            } else if (onlySignedDigits.test(match)) {
                // match is an integer
                if (Number.isSafeInteger(Number(match))) {
                    // if integer is safe, we leave it as-is
                    return match;
                } else {
                    // otherwise convert it to a string of digits
                    return JSON.stringify(match);
                }
            } else {
                // match is a float or scientific notation, we leave it as-is
                return match;
            }
        });
    }

    // Helper function to replace all instances of bigint object comming from Elm in the JSON.
    // {"bigint":"420000000"} -> 420000000
    const bigIntObject = /{"bigint":"(-?\d+)"}/g
    function replaceBigIntObject(compactJsonString) {
        return compactJsonString.replace(bigIntObject, (_match, bigint) => bigint);
    }
}