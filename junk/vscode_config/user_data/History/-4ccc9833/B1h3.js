
const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {

  entry: "./src/index.tsx",
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

  devtool: 'inline-source-map',

  module: {
    rules: [
      {
        test: /\.(png|jpg|gif)$/i,
        type: "asset",
      },
      {
        test: /\.plutus$/i,
        type: "asset/source",
      },
      {
        test: /\.css$/,
        use: ["css-loader"],
      },
      {
        test: /\.(ts|tsx)$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },

  resolve: {
    extensions: [".js", ".jsx", ".tsx", ".ts"],
    fallback: {
      buffer: require.resolve("buffer/"),
      http: false,
      url: false,
      stream: false,
      crypto: false,
      https: false,
      net: false,
      tls: false,
      zlib: false,
      os: false,
      path: false,
      fs: false,
      readline: false,
      child_process: false,
    },
  },
  
  plugins: [
    new HtmlWebpackPlugin({
      title: "mlabs-plutus-scaffold",
      template: path.join(__dirname, "public", "index.html"),
      // inject: false, // See stackoverflow.com/a/38292765/3067181
    }),
  ]
};