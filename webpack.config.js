const path = require("path");
const webpack = require("webpack");

function isProduction(argv) {
  return !argv.development;
}

module.exports = (_, argv) => ({
  entry: "./frontend/src/app.ts",
  mode: isProduction(argv) ? "production" : "development",
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.tsx?$/i,
        loader: "ts-loader",
        options: {
          configFile: "tsconfig-webpack.json",
        },
        exclude: /node_modules/,
      },
    ],
  },
  output: {
    path: path.resolve(__dirname, "frontend/out"),
    filename: "app.js",
  },
  performance: {
    hints: false,
  },
});
