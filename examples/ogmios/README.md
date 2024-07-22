```sh
# Start Ogmios in a container
ogmios \
  --node-socket ~/.dmtr/tmp/myproject/mainnet-stable.socket \
  --node-config ~/git/iohk/cardano-configurations/network/mainnet/cardano-node/config.json \
  --host 0.0.0.0 \
  --include-transaction-cbor
```

```sh
# Start the elm app
npx elm-watch hot
```
