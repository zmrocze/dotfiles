## Using compiled scripts

**Table of Contents**

- [Importing serialized scripts](#importing-serialized-scripts)

### Importing serialized scripts

To use your own scripts, compile them to any subdirectory in the root of your project (where `webpack.config.js` is located) and add a relative path to `webpack.config.js` under the `resolve.alias` section. In CTL, we have the `Scripts` alias for this purpose. Note the capitalization of `Scripts`: it is necessary to disambiguate it from local folders.

First, in your `webpack.config.js`, define an `alias` under `module.exports.resolve.alias` in order to `require` the compiled scripts from JS modules:

```javascript
const path = require("path");

module.exports = {
  // ...
  resolve: {
    modules: [process.env.NODE_PATH],
    extensions: [".js"],
    fallback: {
      // ...
    },
    alias: {
      // You should update this path to the location of your compiled scripts,
      // relative to `webpack.config.js`
      Scripts: path.resolve(__dirname, "fixtures/scripts"),
    },
  },
};
```

You must also add the following to `module.exports.module.rules`:

```javascript
module.exports = {
  // ...
  module: {
    rules: [
      {
        test: /\.plutus$/i,
        type: "asset/source",
      },
      // ...
    ],
  },
};
```

This enables inlining your serialized scripts in `.js` files, to then be loaded in Purescript via the FFI:

```javascript
// inline .plutus file as a string
exports.myscript = require("Scripts/myscript.plutus");
```

And on the purescript side, the script can be loaded like so:

```purescript
foreign import myscript :: String

parseValidator :: Contract () Validator
parseValidator = liftMaybe (error "Error decoding myscript") do
    envelope <- decodeTextEnvelope myscript
    Validator <$> Contract.TextEnvelope.plutusScriptV1FromEnvelope envelope
myContract cfg = runContract_ cfg $ do
  validator <- parseValidator
  ...
```

This way you avoid hardcoding your scripts directly to .purs files which could lead to synchronization issues should your scripts change.

**Note**: The `alias` method above will only work in the browser when bundling with Webpack. In order to load the scripts for both browser and NodeJS environments, you can use the `BROWSER_RUNTIME` environment variable like so:

```javascript
let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/my-script.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/my-script.plutus"),
    "utf8"
  );
}
exports.myScript = script;
```

Note that the relative path passed to `path.resolve` for the NodeJS case starts from the `output` directory that the Purescript compiler produces.
