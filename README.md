# Elm Cardano

Elm offchain package for Cardano. This project aims to be the friendliest and
most productive way of handling an offchain Cardano frontend. It should be a
perfect match to [Aiken][aiken] for onchain code.

[aiken]: https://aiken-lang.org/

## Quickstart

> Remark: this section aspirational as the elm package is not published yet.

Install the elm-cardano CLI and the Elm package.

```sh
# Install the elm compiler and the elm-cardano CLI
npm install -g elm elm-cardano
# Initialize a template project in the elm-cardano-starter/ folder
mkdir elm-cardano-starter && cd elm-cardano-starter
npx elm-cardano init
```

This will generate the following template structure:
```sh
.gitignore   # some files to ignore
elm.json     # the elm app config
index.html   # the app web page
src/Main.elm # the elm app
```

Now you simply need to compile the elm app and start a static server.
```sh
# Compile the elm app. This will create some new files.
npx elm-cardano make src/Main.elm --output main.js
# Start a static web server then open your browser
python -m http.server
```

That’s it you are ready to build a Cardano offchain frontend with Elm!
More examples are available in the `examples/` dir of the repo.
More information about this project is also available
in the different documents in the `docs/` dir of the repo.

## Contributions

Contributions are very welcomed! For now things are moving fast so I suggest
discussing first over [TxPipe discord][txpipe-discord], in the
[#elm][elm-cardano-channel] channel.

[txpipe-discord]: https://discord.gg/ZTHcHUy5HY
[elm-cardano-channel]: https://discord.com/channels/946071061567529010/1168602442657697793

The elm-cardano cli is built using the rust language.
To build successfully, it attempts to statically load the WASM files for Aiken UPLC virtual machine.
So we first need to download these files locally, then we can compile (in release).
```sh
# Download the uplc-wasm archive
curl -LO 'https://github.com/mpizenberg/uplc-wasm/releases/download/v0.2.0/my-artifact.zip'
unzip my-artifact.zip -d cli/pkg-uplc-wasm

# Build the elm-cardano cli
cargo build --release
```

Now you can put that `target/release/elm-cardano` binary somewhere in your path.
Or better, put a link to it somewhere in your path.
After doing that, we can install the elm tools.
The simplest way is via npm.
```sh
# Install all the tools:
# elm, elm-format, elm-test, elm-review, elm-watch
npm install
```

You can then run the tools you need by prefixing the command with `npx`.

```sh
# compile the elm package
npx elm make
# run the tests, using the elm-cardano binary for compilation
npx elm-test --compiler elm-cardano
# review the code
npx elm-review
# run the Tx builder example
cd examples/txbuild && npx elm-watch hot
```
