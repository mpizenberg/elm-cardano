function initElmCardanoJs(app) {
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