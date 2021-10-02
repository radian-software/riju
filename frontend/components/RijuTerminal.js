import Box from "@mui/material/Box";
import React, { useEffect } from "react";
import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";
import "xterm/css/xterm.css";
import { EventEmitter } from "../utils/EventEmitter";

function RijuTerminal() {
  useEffect(() => {
    const term = new Terminal({
      fontFamily: "Fira Code",
      theme: {
        background: "#292D3E",
      },
    });
    term.open(document.getElementById("riju-term"));
    term.write("Connecting to server...");
    const fitAddon = new FitAddon();
    term.loadAddon(fitAddon);

    window.addEventListener("resize", () => fitAddon.fit());
    EventEmitter.subscribe("terminal", (payload) => {
      if (!payload) return;
      const { type, data } = payload;
      switch (type) {
        case "terminalClear":
          term.reset();
          break;
        case "terminalOutput":
          term.write(data);
          break;
        default:
          term.write(data);
          break;
      }
    });
    term.onData((data) => {
      EventEmitter.dispatch("send", { event: "terminalInput", input: data });
    });
  }, []);

  return (
    <Box id="riju-term" sx={{ height: `calc(100% - 8px)`, mt: 1, ml: 2 }} />
  );
}

export default RijuTerminal;
