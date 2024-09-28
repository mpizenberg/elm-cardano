# Smart Bucket

What I call "smart bucket" here, is basically a simple aiken contract,
that enables locking some utxos reusable by anyone, as long as they are put back in.
I call such a utxo a "bucket".
In this example, the bucket is limited to (testnet) ada and iUSD.
But the iUSD policy could be replaced by any policy really.

```sh
# build the aiken contract
aiken build
# build the elm frontend
npx elm-cardano make src/Main.elm --output main.js && python -m http.server
```
