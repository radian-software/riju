const isFn = (callback) => {
  return callback && typeof callback == "function";
};

const createSocket = (url) => {
  const socket = new WebSocket(url);
  return socket;
};

export const SocketManager = {
  socket: null,
  isConnected: false,
  connect: function (config, onOpen, onMessage, onClose) {
    let url =
      "wss://" +
      "riju.codes" +
      `/api/v1/ws?lang=${encodeURIComponent(config.id)}`;
    this.socket = createSocket(url);
    this.socket.addEventListener("open", () => {
      console.log("Successfully connected to server playground");
      this.isConnected = true;
      if (isFn(onOpen)) onOpen();
    });
    this.socket.addEventListener("message", async (event) => {
      if (isFn(onMessage)) onMessage(event);
    });
    this.socket.addEventListener("close", (event) => {
      if (isFn(onClose)) onClose(event);
      this.isConnected = false;
    });
  },
  disconnect: function () {
    if (this.socket) {
      if (this.socket.readyState == WebSocket.OPEN) {
        this.socket.close();
      }
    }
  },
  send: function (data) {
    if (this.socket) {
      if (this.socket.readyState == WebSocket.OPEN) {
        this.socket.send(JSON.stringify(data));
      }
    }
  },
};
