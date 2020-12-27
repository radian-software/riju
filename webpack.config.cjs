const path = require("path");

const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");
const webpack = require("webpack");

function isProduction(argv) {
  return argv.mode !== "development";
}

module.exports = (_, argv) => ({
  devtool: isProduction(argv) ? undefined : "source-map",
  entry: "./frontend/src/app.js",
  mode: isProduction(argv) ? "production" : "development",
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.ttf$/,
        use: ["file-loader"],
      },
      {
        test: /\.js$/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-env"],
          },
        },
        include: /vscode-jsonrpc/,
      },
    ],
  },
  node: {
    net: "mock",
  },
  output: {
    path: path.resolve(__dirname, "frontend/out"),
    publicPath: "/js/",
    filename: "app.js",
  },
  performance: {
    hints: false,
  },
  plugins: [
    new webpack.ProvidePlugin({
      regeneratorRuntime: "regenerator-runtime/runtime",
    }),
    new MonacoWebpackPlugin(),
  ],
  resolve: {
    alias: {
      vscode: require.resolve("monaco-languageclient/lib/vscode-compatibility"),
    },
  },
});
