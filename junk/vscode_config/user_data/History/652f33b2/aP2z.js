
const path = require("path");

module.exports = {

  entry: "./src/index.js",
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "build"),
  },

  plugins: [
    new HtmlWebpackPlugin({
      title: "mlabs-plutus-scaffold",
      template: path.join(__dirname, "public", "index.html"),
      // inject: false, // See stackoverflow.com/a/38292765/3067181
    }),
  ]
};