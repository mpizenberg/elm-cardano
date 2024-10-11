# Interactive Tx Builder

This app acts as a demonstration for the capabilities the transaction builder in elm-cardano.
It aims to be both useful for the general Cardano community as an advanced, interactive Tx builder with a UI,
and useful to showcase ways of using the elm-cardano library.

Todo:

- [x] create transfer intents
- improve transfer intents:
  - [] make it clear that addresses can only be copy-pasted (in hex binary)
  - [] improve token selection (display max amounts, etc.)
- [] add costing of intents (size, mem, cpu)
- [] visualize transfer elements in the cart
- [] create a Tx with all elements in the cart
- [] update available amounts in wallets with elements already in the cart
- create other intents:
  - [] create rewards withdrawal intents
  - [] create spend from contract intents
  - [] create send to contract intents
  - [] create minting intents
  - [] create pool stake delegation intents
  - [] create drep stake delegation intents
  - [] create voting intents
- [] ...
