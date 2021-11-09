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
  connect: function (config, onOpen, onMessage) {
    if (this.socket && this.isConnected) return;
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
  },
  disconnect: function (onClose) {
    // This can be a promise as well.
    const closed = [WebSocket.CLOSING, WebSocket.CLOSED];
    if (this.socket) {
      console.log("Socket", this.socket.readyState);
      if (!closed.includes(this.socket.readyState)) {
        console.log("Socket closing");
        this.socket.addEventListener("close", (event) => {
          if (event.wasClean) {
            console.log("Connection closed cleanly");
          } else {
            console.error("Connection died");
          }
          if (isFn(onClose)) onClose(event);
          this.isConnected = false;
          console.log("Socket closed");
        });
        this.socket.close();
      }
    }
  },
  send: function (data) {
    console.log("Socket send");
    if (this.socket) {
      if (this.socket.readyState == WebSocket.OPEN) {
        console.log("Socket sent");
        this.socket.send(JSON.stringify(data));
      }
    }
  },
};
