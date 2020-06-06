const appRoot = require("app-root-path");

const langs = require(appRoot + "/langs");

const express = require("express");
const sslRedirect = require("heroku-ssl-redirect");

const app = express();
const host = process.env.HOST || "localhost";
const port = parseInt(process.env.PORT) || 6119;

app.use(sslRedirect());
app.get("/:lang", (req, res) => {
  if (langs[req.params.lang]) {
    res.sendFile(appRoot + "/dynamic/app.html");
  } else {
    res.send(`No such language: ${req.params.lang}`);
  }
});
app.use("/", express.static(appRoot + "/static"));

app.listen(port, host, () =>
  console.log(`Listening on http://${host}:${port}`)
);
