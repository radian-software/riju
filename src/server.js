const appRoot = require("app-root-path");
const express = require("express");
const sslRedirect = require("heroku-ssl-redirect");

const app = express();
const host = process.env.HOST || "localhost";
const port = parseInt(process.env.PORT) || 6119;

app.use(sslRedirect());
app.use("/", express.static(appRoot + "/static"));

app.listen(port, host, () =>
  console.log(`Listening on http://${host}:${port}`)
);
