import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import {
  Circle,
  Code as Format,
  Home,
  HorizontalSplit,
  PlayArrow,
  VerticalSplit,
} from "@mui/icons-material";
import { LoadingButton } from "@mui/lab";
import {
  Box,
  Button,
  Chip,
  Stack,
  ToggleButton,
  ToggleButtonGroup,
  Typography,
} from "@mui/material";
import ansi from "ansicolor";
import dynamic from "next/dynamic";
import Head from "next/head";
import { useRouter } from "next/router";
import React, { useEffect, useRef, useState } from "react";
import { createMessageConnection } from "vscode-jsonrpc";
import Layouts from "../../components/Layouts";
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
  const paneRef = useRef(null);
  const [config, setConfig] = useState(langConfig);
  const [mounted, setMounted] = useState(false);
  const [isRunning, setRunning] = useState(false);
  const [isFormatting, setFormatting] = useState(false);
  const [isLspStarted, setLspStarted] = useState(false);
  const [isLspRequested, setIsLspRequested] = useState(false);
  const [splitType, setSplitType] = useState("horizontal");
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
    // Below code is just for adding an empty line in editor
    monaco.languages.registerCodeLensProvider(
      config.monacoLang || "plaintext",
      {
        provideCodeLenses: function (model, token) {
          return {
            lenses: [
              {
                range: {
                  startLineNumber: 1,
                  startColumn: 1,
                  endLineNumber: 2,
                  endColumn: 1,
                },
                id: "Format",
                command: {
                  // id: commandId,
                  // title: "Format",
                  title: "",
                },
              },
            ],
            dispose: () => {},
          };
        },
        resolveCodeLens: function (model, codeLens, token) {
          return codeLens;
        },
      }
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
    if (status != "idle") {
      return;
    } else {
      connect();
    }
  };

  const handleLayout = (event, value) => {
    const e = document.querySelector(".split .gutter");
    e.classList.replace(`gutter-${splitType}`, `gutter-${value}`);
    const es = document.querySelectorAll(".split .panel");
    for (const e of es) {
      e.removeAttribute("style");
      e.removeAttribute("style");
    }
    setSplitType(value);
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
      <Stack
        direction="column"
        sx={{ height: "100vh", bgcolor: "white" }}
        alignItems="stretch"
      >
        <Stack
          direction="row"
          alignItems="center"
          justifyContent="space-between"
          sx={{
            boxShadow: `0 2px 4px rgb(0 0 0 / 10%)`,
            zIndex: (t) => t.zIndex.appBar,
            height: "48px",
            p: "0 24px",
          }}
        >
          <Stack direction="row" alignItems="center" gap={2}>
            <Button
              variant="contained"
              disableElevation
              size="medium"
              color="primary"
              onClick={() => {
                router.push("/");
              }}
            >
              <Home fontSize={"small"} />
            </Button>
            <Typography sx={{ fontSize: 14, fontWeight: 600 }}>
              {config.name}
            </Typography>
            <Chip
              size="small"
              variant="outlined"
              color={status != "idle" ? "info" : "default"}
              sx={{
                fontSize: "0.7rem",
                height: "16px",
              }}
              label={status}
            />
          </Stack>
          <Stack direction="row" gap={1} alignItems="center">
            <LoadingButton
              onClick={handleLspClick}
              size="medium"
              variant="text"
              loading={isLspRequested}
              sx={{
                visibility: config.lsp ? "visible" : "hidden",
                mr: 1,
                color: (t) =>
                  isLspStarted
                    ? t.palette.success.main
                    : t.palette.text.disabled,
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
            <ToggleButtonGroup
              size="small"
              value={splitType}
              color="primary"
              exclusive
              onChange={handleLayout}
              aria-label="split type"
            >
              <ToggleButton
                value="vertical"
                aria-label="vertical split"
                sx={{ border: "none" }}
              >
                <VerticalSplit fontSize="small" />
              </ToggleButton>
              <ToggleButton
                value="horizontal"
                aria-label="horizontal split"
                sx={{ border: "none" }}
              >
                <HorizontalSplit fontSize="small" />
              </ToggleButton>
            </ToggleButtonGroup>

            {config.format && (
              <LoadingButton
                onClick={sendFormat}
                loading={isFormatting}
                size="medium"
                color="primary"
                variant="contained"
                disableElevation
                endIcon={<Format fontSize="small" />}
              >
                <Typography sx={{ fontSize: 12 }}>Prettify</Typography>
              </LoadingButton>
            )}
            <LoadingButton
              onClick={showValue}
              loading={isRunning}
              size="medium"
              color="success"
              variant="contained"
              disableElevation
              endIcon={<PlayArrow fontSize="small" htmlColor="#fff" />}
            >
              <Typography sx={{ fontSize: 12, color: "#fff" }}>Run</Typography>
            </LoadingButton>
          </Stack>
        </Stack>
        <Layouts splitType={splitType}>
          <Box className="panel editor">
            <MonacoEditor
              wrapperClassName={"rijuEditor"}
              onChange={handleChange}
              language={config.monacoLang || "plaintext"}
              value={config.template + "\n"}
              options={{
                minimap: { enabled: splitType == "horizontal" ? false : true },
                scrollbar: { verticalScrollbarSize: 0 },
                fontLigatures: true,
                fontFamily: "Fira Code",
              }}
              onMount={editorDidMount}
            />
          </Box>
          <Box className="panel" sx={{ bgcolor: "#292D3E", p: 2 }}>
            <RijuTerminal />
          </Box>
        </Layouts>
      </Stack>
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
//     props: { langConfig: lsp }, // will be passed to the page component as props
//   };
// }

export default CodeRunner;
