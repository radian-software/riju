const path = require("path");

module.exports = {
  entry: "./frontend/src/app.ts",
  mode: process.env.NODE_ENV || "production",
  output: {
    path: path.resolve(__dirname, "frontend/out"),
    filename: "app.js",
  },
};
