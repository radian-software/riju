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
    const fitAddon = new FitAddon();
    term.loadAddon(fitAddon);
    term.open(document.getElementById("riju-term"));
    term.write("Connecting to server...");

    window.addEventListener("resize", () => fitAddon.fit());
    const token1 = EventEmitter.subscribe("resize", () => {
      const event = new Event("resize");
      window.dispatchEvent(event);
    });
    const token2 = EventEmitter.subscribe("terminal", (payload) => {
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

    () => EventEmitter.unsubcribe(token1, token2);
  }, []);

  return (
    <Box
      id="riju-term"
      sx={{
        height: "100%",
        width: "100%",
        backgroundColor: "#292D3E",
      }}
    />
  );
}

export default RijuTerminal;
