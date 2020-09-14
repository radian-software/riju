const path = require("path");

const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");

function isProduction(argv) {
  return !argv.development;
}

module.exports = (_, argv) => ({
  devtool: isProduction(argv) ? undefined : "source-map",
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
  plugins: [new MonacoWebpackPlugin()],
  resolve: {
    alias: {
      vscode: require.resolve("monaco-languageclient/lib/vscode-compatibility"),
    },
    extensions: [".js", ".ts"],
  },
});
