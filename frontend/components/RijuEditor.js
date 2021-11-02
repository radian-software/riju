import MonacoEditor from "@monaco-editor/react";
import {
  createConnection,
  MonacoLanguageClient,
  MonacoServices,
  Services,
} from "monaco-languageclient";
import React from "react";
import { createMessageConnection } from "vscode-jsonrpc";
import RijuMessageReader from "../services/RijuMessageReader";
import RijuMessageWriter from "../services/RijuMessageWriter";
import { SocketManager } from "../services/WS";
import { EventEmitter } from "../utils/EventEmitter";

let clientDisposable = null;
let servicesDisposable = null;

const RijuEditor = (props) => {
  const { onEditorValueChange, config, splitType, onEditorMount } = props;
  const editorRef = React.useRef();
  const monacoRef = React.useRef();

  React.useEffect(() => {
    const token1 = EventEmitter.subscribe("lspStarted", (data) => {
      const { message } = data;
      initLSP(message, monacoRef.current, editorRef.current);
    });
    const token2 = EventEmitter.subscribe("lspStopped", () => {
      if (clientDisposable) {
        clientDisposable.dispose();
        clientDisposable = null;
      }
      if (servicesDisposable) {
        servicesDisposable.dispose();
        servicesDisposable = null;
      }
    });
    () => {
      EventEmitter.unsubcribe(token1, token2);
      if (clientDisposable) {
        clientDisposable.dispose();
        clientDisposable = null;
      }
      if (servicesDisposable) {
        servicesDisposable.dispose();
        servicesDisposable = null;
      }
    };
  }, []);

  const initLSP = (message, monaco, editor) => {
    const socket = SocketManager.socket;
    const services = MonacoServices.create(editor, {
      rootUri: `file://${message.root}`,
    });

    servicesDisposable = Services.install(services);
    const newURI = `file://${message.root}/${config.main}`;
    const oldModel = editor.getModel();
    // if (oldModel.uri.toString() !== newURI) {
    // This code is likely to be buggy as it will probably
    // never run and has thus never been tested.
    // editor.setModel(
    //   monaco.editor.createModel(
    //     oldModel?.getValue(),
    //     undefined,
    //     monaco.Uri.parse(newURI)
    //   )
    // );
    // oldModel.dispose();
    // }

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
  };

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
