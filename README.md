# Elm Cardano

Elm offchain package for Cardano. This project aims to be the friendliest and
most productive way of handling an offchain Cardano frontend. It should be a
perfect match to [Aiken][aiken] for onchain code.

[aiken]: https://aiken-lang.org/

## Usage

> Remark: this is aspiration as the elm-cardano package is not published yet.

Install the Elm package with your usual tool. We recommand using
[elm-json][elm-json].

[elm-json]: https://github.com/zwilias/elm-json

```sh
# Install 
elm install mpizenberg/elm-cardano
# Or better, use elm-json
elm-json install mpizenberg/elm-cardano
```

Once the elm-cardano package is installed in your elm project, you need to add
the corresponding JS file `elm-cardano.js` to your `index.html`. Finally, you
need to call `initElmCardanoJs(app)` after initializing the main of the elm app.

> TODO: this file currently lives in `examples/wallet-cip30/elm-cardano.js` we
> should bring it up in the repo once things have stabilized a bit.

```html
<html>
<head>
    <meta charset="UTF-8">
    <title>Main</title>
    <script src="main.js"></script>
    <script src="elm-cardano.js"></script>
</head>
<body>
    <div id="myapp"></div>
    <script>
        var app = Elm.Main.init({
            node: document.getElementById('myapp'),
            flags: null,
        });
        initElmCardanoJs(app)
    </script>
</body>
</html>
```

After that, just follow the elm-cardano package docs to know how to use it.

## Contributions

Contributions are welcomed! For now things are moving fast so I suggest
discussing first over [TxPipe discord][txpipe-discord], in the
[Elm Cardano thread][elm-cardano-thread].

[txpipe-discord]: https://discord.gg/ZTHcHUy5HY
[elm-cardano-thread]: https://discord.com/channels/946071061567529010/1162410032697188442

The tools needed for this repo are all installed via npm.

```sh
# Install all the tools:
# elm, elm-format, elm-test-rs, elm-review, elm-watch
npm install
```

You can then run the tools you need by prefixing the command with `npx`.

```sh
# compile the elm package
npx elm make
# run the tests
npx elm-test-rs
# review the code
npx elm-review
# run the CIP30 example
cd examples/wallet-cip30 && npx elm-watch hot
```
