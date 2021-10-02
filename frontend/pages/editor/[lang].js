import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { Circle, Code as Format, Home, PlayArrow } from "@mui/icons-material";
import { LoadingButton } from "@mui/lab";
import { Box, Button, Chip, Divider, Typography } from "@mui/material";
import ansi from "ansicolor";
import dynamic from "next/dynamic";
import Head from "next/head";
import { useRouter } from "next/router";
import React, { useEffect, useRef, useState } from "react";
import { createMessageConnection } from "vscode-jsonrpc";
import langs from "../../static/langs.json";
import { EventEmitter } from "../../utils/EventEmitter";
ansi.rgb = {
  green: "#00FD61",
};
const RijuTerminal = dynamic(() => import("../../components/RijuTerminal"), {
  ssr: false,
});

const DEBUG = true;
let clientDisposable = null;
let servicesDisposable = null;
let serviceLogBuffers = {};
let serviceLogLines = {};

const CodeRunner = (props) => {
  const router = useRouter();
  const { langConfig } = props;
  const editorRef = useRef(null);
  const [config, setConfig] = useState(langConfig);
  const [mounted, setMounted] = useState(false);
  const [isRunning, setRunning] = useState(false);
  const [isFormatting, setFormatting] = useState(false);
  const [isLspStarted, setLspStarted] = useState(false);
  const [isLspRequested, setIsLspRequested] = useState(false);
  const monaco = useMonaco();
  const [status, setStatus] = useState("connecting");

  function sendToTerminal(type, data) {
    EventEmitter.dispatch("terminal", { type, data });
  }

  function connect() {
    setStatus("connecting");
    const socket = new WebSocket(
      // (document.location.protocol === "http:" ? "ws://" : "wss://") +
      "wss://" +
        "riju.codes" +
        `/api/v1/ws?lang=${encodeURIComponent(config.id)}`
    );
    socket.addEventListener("open", () => {
      console.log("Successfully connected to server playground");
      setStatus("connected");
    });
    EventEmitter.subscribe("send", (payload) => {
      if (DEBUG) {
        console.log("SEND:", payload);
      }
      if (socket) {
        socket.send(JSON.stringify(payload));
      }
    });
    socket.addEventListener("message", async (event) => {
      let message;
      try {
        message = JSON.parse(event.data);
      } catch (err) {
        console.error("Malformed message from server:", event.data);
        return;
      }
      if (
        DEBUG &&
        message &&
        message.event !== "lspOutput" &&
        message.event !== "serviceLog"
      ) {
        console.log("RECEIVE:", message);
      }
      if (message && message.event && message.event !== "error") {
        // retryDelayMs = initialRetryDelayMs;
      }
      switch (message && message.event) {
        case "terminalClear":
          // term.reset();
          sendToTerminal("terminalClear");
          return;
        case "terminalOutput":
          if (typeof message.output !== "string") {
            console.error("Unexpected message from server:", message);
            return;
          }
          sendToTerminal("terminalOutput", ansi.white(message.output));
          setRunning(false);
          return;
        case "formattedCode":
          setFormatting(false);
          if (
            typeof message.code !== "string" ||
            typeof message.originalCode !== "string"
          ) {
            console.error("Unexpected message from server:", message);
            return;
          }
          if (editorRef.current?.getValue() === message.originalCode) {
            editorRef.current?.setValue(message.code);
          }
          return;
        case "lspStopped":
          setIsLspRequested(false);
          setLspStarted(false);
          if (clientDisposable) {
            clientDisposable.dispose();
            clientDisposable = null;
          }
          if (servicesDisposable) {
            servicesDisposable.dispose();
            servicesDisposable = null;
          }
          break;
        case "lspStarted":
          setLspStarted(true);
          setIsLspRequested(false);
          if (typeof message.root !== "string") {
            console.error("Unexpected message from server:", message);
            return;
          }

          console.log("Started", message.root, config.main);
          // EventEmitter.dispatch("lspStarted", message);
          const {
            createConnection,
            MonacoLanguageClient,
            MonacoServices,
            Services,
          } = await import("monaco-languageclient");
          const services = MonacoServices.create(editorRef.current, {
            rootUri: `file://${message.root}`,
          });
          servicesDisposable = Services.install(services);
          const newURI = `file://${message.root}/${config.main}`;
          const oldModel = editorRef.current.getModel();
          console.log("Check 4", oldModel.uri, newURI);
          if (oldModel.uri.toString() !== newURI) {
            // This code is likely to be buggy as it will probably
            // never run and has thus never been tested.
            editorRef.current.setModel(
              monaco.editor.createModel(
                oldModel.getValue(),
                undefined,
                monaco.Uri.parse(newURI)
              )
            );
            oldModel.dispose();
          }

          const RijuMessageReader = (
            await import("../../services/RijuMessageReader")
          ).default;
          const RijuMessageWriter = (
            await import("../../services/RijuMessageWriter")
          ).default;

          const connection = createMessageConnection(
            new RijuMessageReader(socket),
            new RijuMessageWriter(socket, config)
          );
          const client = new MonacoLanguageClient({
            name: "Riju",
            clientOptions: {
              documentSelector: [{ pattern: "**" }],
              middleware: {
                workspace: {
                  configuration: (params, token, configuration) => {
                    return Array(configuration(params, token).length).fill(
                      config.lsp.config !== undefined ? config.lsp.config : {}
                    );
                  },
                },
              },
              initializationOptions: config.lsp.init || {},
            },
            connectionProvider: {
              get: (errorHandler, closeHandler) =>
                Promise.resolve(
                  createConnection(connection, errorHandler, closeHandler)
                ),
            },
          });
          clientDisposable = client.start();
          return;
        case "lspOutput":
          // Should be handled by RijuMessageReader
          return;
        case "serviceLog":
          if (
            typeof message.service !== "string" ||
            typeof message.output !== "string"
          ) {
            console.error("Unexpected message from server:", message);
            return;
          }
          let buffer = serviceLogBuffers[message.service] || "";
          let lines = serviceLogLines[message.service] || [];
          buffer += message.output;
          while (buffer.includes("\n")) {
            const idx = buffer.indexOf("\n");
            const line = buffer.slice(0, idx);
            buffer = buffer.slice(idx + 1);
            lines.push(line);
            if (DEBUG) {
              console.log(`${message.service.toUpperCase()} || ${line}`);
            }
          }
          serviceLogBuffers[message.service] = buffer;
          serviceLogLines[message.service] = lines;
          return;
        case "serviceFailed":
          if (
            typeof message.service !== "string" ||
            typeof message.error !== "string"
          ) {
            console.error("Unexpected message from server:", message);
            return;
          }
          switch (message.service) {
            case "formatter":
              setFormatting(false);
              // showError({
              //   message: "Could not prettify code!",
              //   data: serviceLogLines["formatter"].join("\n"),
              // });
              break;
            case "lsp":
              setLspStarted(false);
              setIsLspRequested(false);
              // lspButton.classList.add("is-light");
              // lspButtonState.innerText = "CRASHED";
              break;
            case "terminal":
              sendToTerminal(
                "terminalOutput",
                ansi.red(`\r\n[${message.error}]`)
              );
              break;
          }
          return;
        default:
          console.error("Unexpected message from server:", message);
      }
    });
    socket.addEventListener("close", (event) => {
      if (event.wasClean) {
        console.log("Connection closed cleanly");
      } else {
        console.error("Connection died");
      }
      if (clientDisposable) {
        clientDisposable.dispose();
        clientDisposable = null;
      }
      if (servicesDisposable) {
        servicesDisposable.dispose();
        servicesDisposable = null;
      }
      setRunning(false);
      setLspStarted(false);
      setIsLspRequested(false);
      setStatus("idle");
    });

    return socket;
  }

  useEffect(() => {
    if (!config || !mounted) return;
    const socket = connect();
    return () => socket && socket.close();
  }, [config, mounted]);

  function showValue() {
    setRunning(true);
    EventEmitter.dispatch("send", {
      event: "runCode",
      code: editorRef.current.getValue(),
    });
  }

  function sendFormat() {
    setFormatting(true);
    serviceLogBuffers["formatter"] = "";
    serviceLogLines["formatter"] = [];
    EventEmitter.dispatch("send", {
      event: "formatCode",
      code: editorRef.current.getValue(),
    });
  }

  function editorDidMount(editor, monaco) {
    editorRef.current = editor;

    editor.addAction({
      id: "runCode",
      label: "Run",
      keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
      contextMenuGroupId: "2_execution",
      run: () => {
        showValue();
      },
    });
    // editor.getModel().onDidChangeContent(() => recordActivity());
    // window.addEventListener("resize", () => editor.layout());
    editor.getModel().setValue(config.template + "\n");
    monaco.editor.setModelLanguage(
      editor.getModel(),
      config.monacoLang || "plaintext"
    );
    setMounted(true);
  }

  const handleLspClick = () => {
    setIsLspRequested(true);
    if (isLspStarted) {
      EventEmitter.dispatch("send", {
        event: "lspStop",
      });
    } else {
      EventEmitter.dispatch("send", {
        event: "lspStart",
      });
    }
  };

  const handleChange = () => {
    if (isConnected) {
      return;
    } else {
      connect();
    }
  };

  return (
    <>
      <Head>
        <title>Riju</title>
        <meta
          name="description"
          content="Riju - fast playground for any language"
        />
        <link rel="icon" href="/favicon.ico" />
        <meta
          name="viewport"
          content="minimum-scale=1, initial-scale=1, width=device-width"
        />
      </Head>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          height: "100vh",
          bgcolor: "#fff",
        }}
        component="main"
      >
        <Box
          sx={{
            display: "flex",
            flexDirection: "row",
            alignItems: "stretch",
            boxShadow: `0 2px 4px rgb(0 0 0 / 2%)`,
            width: "60%",
          }}
        >
          <Box sx={{ flexGrow: 1, display: "flex", alignItems: "center" }}>
            <Button
              variant="contained"
              sx={{ borderRadius: 0, minWidth: 0 }}
              disableElevation
              size="small"
              color="primary"
              onClick={() => {
                router.push("/");
              }}
            >
              <Home fontSize={"small"} />
            </Button>
            <Typography sx={{ fontSize: 14, px: 2, fontWeight: 600 }}>
              {config.name}
            </Typography>
            <Chip
              size="small"
              variant="outlined"
              color="info"
              sx={{
                fontSize: "0.7rem",
                alignSelf: "center",
                height: "16px",
              }}
              label={status}
            />
          </Box>
          <LoadingButton
            onClick={handleLspClick}
            size="small"
            variant="text"
            loading={isLspRequested}
            sx={{
              borderRadius: 0,
              visibility: config.lsp ? "visible" : "hidden",
              mr: 1,
              color: (t) =>
                isLspStarted ? t.palette.success.main : t.palette.text.disabled,
            }}
            disableElevation
            endIcon={
              <Circle
                fontSize="small"
                sx={{
                  color: "inherit",
                  fontSize: "0.6em !important",
                }}
              />
            }
          >
            <Typography sx={{ fontSize: 12 }}>Autocomplete</Typography>
          </LoadingButton>
          <LoadingButton
            onClick={sendFormat}
            loading={isFormatting}
            size="small"
            color="primary"
            variant="contained"
            sx={{
              borderRadius: 0,
              visibility: config.format ? "visible" : "hidden",
            }}
            disableElevation
            endIcon={<Format fontSize="small" />}
          >
            <Typography sx={{ fontSize: 12 }}>Prettify</Typography>
          </LoadingButton>
          <Divider orientation="vertical" />
          <LoadingButton
            onClick={showValue}
            loading={isRunning}
            size="small"
            color="success"
            variant="contained"
            sx={{ borderRadius: 0 }}
            disableElevation
            endIcon={<PlayArrow fontSize="small" htmlColor="#fff" />}
          >
            <Typography sx={{ fontSize: 12, color: "#fff" }}>Run</Typography>
          </LoadingButton>
        </Box>
        <Divider />
        <Box
          sx={{
            display: "flex",
            flexDirection: "row",
            flexGrow: 1,
            alignItems: "stretch",
          }}
        >
          <Box sx={{ backgroundColor: "white", width: "60%" }}>
            <Box
              component={MonacoEditor}
              wrapperClassName={"rijuEditor"}
              onChange={handleChange}
              height="90vh"
              defaultLanguage="javascript"
              defaultValue="// some comment"
              options={{
                minimap: { enabled: false },
                scrollbar: { verticalScrollbarSize: 0 },
                fontLigatures: true,
                fontFamily: "Fira Code",
              }}
              onMount={editorDidMount}
            />
          </Box>
          <Box
            sx={{
              overflow: "hidden",
              backgroundColor: "#292D3E",
              width: "40%",
            }}
          >
            <RijuTerminal />
          </Box>
        </Box>
      </Box>
    </>
  );
};

CodeRunner.getInitialProps = async (ctx) => {
  const { req, query } = ctx;
  console.log("Query", query);
  let config = langs.javascript;
  if (query.lang) config = langs[query.lang];
  return {
    langConfig: config, // will be passed to the page component as props
  };
};

// export async function getServerSideProps(ctx) {
//   // TODO: Fetch language details using api route
//   const { req, query } = ctx;
//   let lsp = langs.javascript;
//   if (query.lang) lsp = langs[query.lang];
//   return {
//     props: { lsp }, // will be passed to the page component as props
//   };
// }

export default CodeRunner;
