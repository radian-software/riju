import MonacoEditor from "@monaco-editor/react";
import React from "react";
import { LSP } from "../services/LSP";
import { SocketManager } from "../services/WS";
import { EventEmitter } from "../utils/EventEmitter";

const RijuEditor = (props) => {
  const {
    onEditorValueChange,
    config,
    splitType,
    onEditorMount,
    onWsOpen,
    onWsMessage,
    onWsClose,
  } = props;
  const editorRef = React.useRef();
  const monacoRef = React.useRef();

  React.useEffect(() => {
    let token1, token2;
    if (config) {
      token1 = EventEmitter.subscribe("lspStarted", (data) => {
        const { message } = data;
        LSP.init(config, message, monacoRef.current, editorRef.current);
      });
      token2 = EventEmitter.subscribe("lspStopped", () => {
        LSP.dispose();
      });
      SocketManager.connect(config, onWsOpen, onWsMessage);
    }
    return () => {
      LSP.dispose();
      EventEmitter.unsubscribe(token1, token2);
      SocketManager.disconnect();
      if (onWsClose && typeof onWsClose == "function") onWsClose();
    };
  }, [config]);

  return (
    <>
      <MonacoEditor
        wrapperClassName={"rijuEditor"}
        onChange={onEditorValueChange}
        language={config.monacoLang || "plaintext"}
        value={config.template + "\n"}
        options={{
          minimap: { enabled: splitType == "horizontal" ? false : true },
          scrollbar: { verticalScrollbarSize: 0 },
          fontLigatures: true,
          fontFamily: "Fira Code",
        }}
        onMount={(editor, monaco) => {
          editorRef.current = editor;
          monacoRef.current = monaco;
          if (onEditorMount) onEditorMount(editor, monaco);
        }}
      />
    </>
  );
};

export default RijuEditor;
