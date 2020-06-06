import * as appRoot from "app-root-path";
import * as express from "express";
import * as ws from "express-ws";
import * as sslRedirect from "heroku-ssl-redirect";

import * as api from "./api";
import { langs } from "./langs";

const app = ws(express()).app;
const host = process.env.HOST || "localhost";
const port = parseInt(process.env.PORT) || 6119;

app.use(sslRedirect());
app.get("/", (_, res) => {
  res.sendFile(appRoot.path + "/frontend/pages/index.html");
});
app.get("/:lang", (req, res) => {
  if (langs[req.params.lang]) {
    res.sendFile(appRoot.path + "/frontend/pages/app.html");
  } else {
    res.send(`No such language: ${req.params.lang}`);
  }
});
app.use("/css", express.static(appRoot.path + "/frontend/styles"));
app.use("/js", express.static(appRoot.path + "/frontend/out"));
app.use("/api/v1/ws", (req, res, next) => {
  if (!req.query.lang) {
    res.status(400);
    res.send("No language specified");
  } else if (!langs[req.query.lang as string]) {
    res.status(400);
    res.send(`No such language: ${req.query.lang}`);
  } else {
    return next();
  }
});
app.ws("/api/v1/ws", (ws, req) => {
  new api.Session(ws, req.query.lang);
});

app.listen(port, host, () =>
  console.log(`Listening on http://${host}:${port}`)
);
