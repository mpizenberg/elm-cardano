function initElmCardanoJs(app) {
    app.ports.toOgmios.subscribe(async (value) => {
        // console.log("Received value destined to ogmios:", value)
        try {
            if (value.requestType == "ogmios-connect") {
                const websocket = await connectToOgmios(value.websocketAddress, value.connectionId)
                app.ports.fromOgmios.send({responseType: "ogmios-connect", ws: websocket, connectionId: value.connectionId})
            } else if (value.requestType == "ogmios-disconnect") {
                value.ws.close()
            } else if (value.requestType == "ogmios-api") {
                handleApiRequest(value)
            }
        } catch (error) {
            app.ports.fromOgmios.send({responseType: "ogmios-error", error: error})
        }
    })

    async function connectToOgmios(address, connectionId) {
        const client = new WebSocket(address)
        // Listen for messages
        client.addEventListener("message", (event) => {
            // console.log("Message from ogmios (", typeof event.data, "):", event.data)
            // TODO: correctly handle big integers
            const parsedMessage = JSON.parse(event.data)
            app.ports.fromOgmios.send({responseType: "ogmios-message", connectionId, message: parsedMessage})
        });
        // Listen for possible errors
        client.addEventListener("error", (event) => {
            console.log("WebSocket error: ", event)
            app.ports.fromOgmios.send({responseType: "ogmios-error", connectionId, error: event})
        });
        // Listen for closed connection
        client.addEventListener("close", (event) => {
            console.log("The connection has been closed successfully.")
            app.ports.fromOgmios.send({responseType: "ogmios-disconnect", connectionId})
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
    async function handleApiRequest({ws, request}) {
        ws.send(JSON.stringify(request))
    }
}