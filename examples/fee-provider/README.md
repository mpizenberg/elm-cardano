# Vote using Script with external fee and collateral provider

This is an advanced example doing the following things:

1. Change the stake part of some ada to the script credential
2. Register the script as a DRep
3. Vote with the script
4. Ask another wallet to pay the fees and provide a collateral

In theory, we could do all 4 actions in the same transaction.
But we want to demonstrate clearly that another wallet is paying the fees and providing the collateral.
Steps (1) and (2) require our key signature, both for changing the address and paying the registration deposit.
So we will do steps (1) and (2) in a first transaction.
Then we do steps (3) and (4) in a second transaction, where we ask a second wallet to pay for the fees and provide the collateral.

## The aiken script

The aiken script is very minimalist.
Basically any validation requires a signature from our main wallet stake credential.
Not the key credential though, because we want to make sure that the fee and collateral come from an external provider, and not our UTxOs.

```sh
# Build the script -> generate the plutus.json blueprint
aiken build
```

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
