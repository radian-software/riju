import {
    createConnection,
    MonacoLanguageClient,
    MonacoServices,
    Services
} from "monaco-languageclient";
import { createMessageConnection } from "vscode-jsonrpc";
import RijuMessageReader from "./RijuMessageReader";
import RijuMessageWriter from "./RijuMessageWriter";
import { SocketManager } from "./WS";


export const LSP = {
  servicesDisposable: null,
  clientDisposable: null,
  init: function (config, message, monaco, editor) {
    const socket = SocketManager.socket;
    const services = MonacoServices.create(editor, {
      rootUri: `file://${message.root}`,
    });

    this.servicesDisposable = Services.install(services);
    const newURI = `file://${message.root}/${config.main}`;
    const oldModel = editor.getModel();
    let value = "";
    if (oldModel) value = oldModel.getValue();
    editor.setModel(
      monaco.editor.createModel(value, undefined, monaco.Uri.parse(newURI))
    );
    if (oldModel) oldModel.dispose();

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
    this.clientDisposable = client.start();
  },

  dispose: function () {
    if (this.clientDisposable) {
      this.clientDisposable.dispose();
      this.clientDisposable = null;
    }
    if (this.servicesDisposable) {
      this.servicesDisposable.dispose();
      this.servicesDisposable = null;
    }
  },
};
