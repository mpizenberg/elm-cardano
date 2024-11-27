# Examples

This folder contains example apps, making use of the elm-cardano framework.
These examples are the following:

- Examples for learning how to use elm-cardano:
  - aiken-hello-world: Aiken hello world, locking and unlocking 2 ada into an aiken contract
  - smart-bucket: Aiken example where you define a "bucket" that any one can use to send you funds without minFee ada
  - fee-provider: Variant of the aiken-hello-world where the fee and the collateral for the unlocking Tx are provided by an external source
  - vote-script: Register/Vote/Deregister with a script DRep
  - vote-native-script: Register/Vote/Deregister with a native script DRep
- Examples for testing purposes:
  - wallet-cip30: tries all CIP30 endpoints to check the implementation against all wallets
  - ogmios: use Ogmios to walk through the whole chain and decode all transactions that exist
  - txbuild: test the Tx builder and visualize the resulting transactions

Follow the specific instruction of each example to build them.

> **Remark:** these examples do not use elm-cardano as a package dependency.
> This is a limitation of the elm language, that only supports published packages.
> Instead we simply add the package source directory to the example sources,
> and add the package dependencies to the example dependencies.
