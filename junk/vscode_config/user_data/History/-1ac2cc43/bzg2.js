
const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {

  entry: "./src/index.js",
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "build"),
  },

  devServer: {
    static: {
      directory: path.join(__dirname, "build"),
    },
    port: 4008,
  },

  resolve: {
    extensions: [".js"],
  },

  plugins: [
    new HtmlWebpackPlugin({
      title: "mlabs-plutus-scaffold",
      template: path.join(__dirname, "public", "index.html"),
      // inject: false, // See stackoverflow.com/a/38292765/3067181
    }),
  ]
};