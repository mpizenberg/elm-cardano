# Vote using Native Script with a fee provider

This is an advanced example doing the following things:

1. Register a native script as a DRep
2. Vote with the native script, fees payed by an external provider
3. Deregister the native script

## The elm-cardano frontend

The web frontend is composed of two independant Elm apps, compiled to a single bundle.
The first one, `src/Main.elm` contains the main logic, for the person executing the steps in the introduction.
The second one, `src/External.elm` emulates an external provider for the fees and collateral.

Both Elm apps will be connected to a different wallet.
They will communicate with each other via ports, at two occasions.

1. When the main app asks for available UTxOs in preparation for Tx building
2. When the main app asks the other one to sign the prepared transaction

This communication via ports is a good simulation of how the main app would communicate to a backend server.
But here the advantage is that all this all happens in the frontend so it’s easier to set up.

```sh
# Build the frontend and start a static server
npx elm-cardano make src/Main.elm src/External.elm --output main.js && python -m http.server
```

https://github.com/user-attachments/assets/1340f4b1-06a1-4936-a076-2008094a8e9f
