```sh
# Start Dolos in "serve" mode
cd path/to/dolos-mainnet-dir
dolos serve

# Start Ogmios in a container
ogmios \
  --node-socket /path/to/dolos.socket \
  --node-config ~/git/iohk/cardano-configurations/network/mainnet/cardano-node/config.json \
  --host 0.0.0.0 \
  --include-transaction-cbor
```

```sh
# Start the elm app
# npx elm-watch hot
elm-cardano make src/Main.elm --output main.js && python -m http.server
```
