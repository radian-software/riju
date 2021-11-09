import {
  Circle,
  Code as Format,
  Home,
  HorizontalSplit,
  PlayArrow,
  VerticalSplit
} from "@mui/icons-material";
import { LoadingButton } from "@mui/lab";
import {
  Box,
  Button,
  Chip,
  Stack,
  ToggleButton,
  ToggleButtonGroup,
  Typography
} from "@mui/material";
import ansi from "ansicolor";
import dynamic from "next/dynamic";
import Head from "next/head";
import { useRouter } from "next/router";
import React, { useEffect, useRef, useState } from "react";
import langs from "../../assets/langs.json";
import Layouts from "../../components/Layouts";
import { SocketManager } from "../../services/WS";
import { EventEmitter } from "../../utils/EventEmitter";
ansi.rgb = {
  green: "#00FD61",
};
const RijuTerminal = dynamic(() => import("../../components/RijuTerminal"), {
  ssr: false,
});
const RijuEditor = dynamic(() => import("../../components/RijuEditor"), {
  ssr: false,
});

const DEBUG = true;
let serviceLogBuffers = {};
let serviceLogLines = {};

const CodeRunner = (props) => {
  const router = useRouter();
  const { langConfig } = props;
  const editorRef = useRef(null);
  const [config] = useState(langConfig);
  const [mounted, setMounted] = useState(false);
  const [isRunning, setRunning] = useState(false);
  const [isFormatting, setFormatting] = useState(false);
  const [isLspStarted, setLspStarted] = useState(false);
  const [isLspRequested, setIsLspRequested] = useState(false);
  const [splitType, setSplitType] = useState("horizontal");

  const [status, setStatus] = useState("connecting");

  function sendToTerminal(type, data) {
    EventEmitter.dispatch("terminal", { type, data });
  }

  const handleWsOpen = (event) => {
    setStatus("connected");
  };

  const handleWsMessage = (event) => {
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
        EventEmitter.dispatch("lspStopped");
        break;
      case "lspStarted":
        setLspStarted(true);
        setIsLspRequested(false);
        if (typeof message.root !== "string") {
          console.error("Unexpected message from server:", message);
          return;
        }

        EventEmitter.dispatch("lspStarted", { message });

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
            EventEmitter.dispatch("lspStopped");
            break;
          case "terminal":
            sendToTerminal(
              "terminalOutput",
              ansi.red(`\r\n[${message.error}]`)
            );
            break;
        }
        return;
      case "langConfig":
        console.log("Lang Config", message);
        // We could use this message instead of hardcoding the
        // language config into the HTML page returned from the
        // server, but for now we just ignore it.
        return;
      default:
        console.error("Unexpected message from server:", message);
    }
  };

  const handleWsClose = () => {
    EventEmitter.dispatch("lspStopped");
    setRunning(false);
    setLspStarted(false);
    setIsLspRequested(false);
    setStatus("idle");
  };

  useEffect(() => {
    if (!config || !mounted) return;
    serviceLogBuffers = {};
    serviceLogLines = {};
    setStatus("connecting");
  }, [config, mounted]);

  function showValue() {
    setRunning(true);
    SocketManager.send({
      event: "runCode",
      code: editorRef.current.getValue(),
    });
  }

  function sendFormat() {
    setFormatting(true);
    serviceLogBuffers["formatter"] = "";
    serviceLogLines["formatter"] = [];
    SocketManager.send({
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
    setMounted(true);
  }

  const handleLspClick = () => {
    setIsLspRequested(true);
    if (isLspStarted) {
      SocketManager.send({
        event: "lspStop",
      });
    } else {
      SocketManager.send({
        event: "lspStart",
      });
    }
  };

  const handleChange = () => {
    if (SocketManager.isConnected) {
      return;
    } else {
      if (!SocketManager.isConnected) {
        SocketManager.connect(
          config,
          handleWsOpen,
          handleWsMessage,
          handleWsClose
        );
      }
    }
  };

  const handleLayout = (event, value) => {
    const e = document.querySelector(".split .gutter");
    e.classList.replace(`gutter-${splitType}`, `gutter-${value}`);
    const es = document.querySelectorAll(".split .panel");
    for (const e of es) {
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
            minHeight: "48px",
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
            <RijuEditor
              onEditorValueChange={handleChange}
              config={config}
              splitType={splitType}
              onEditorMount={editorDidMount}
              onWsOpen={handleWsOpen}
              onWsMessage={handleWsMessage}
              onWsClose={handleWsClose}
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
