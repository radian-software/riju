export const EventEmitter = {
  events: {},
  dispatch: function (event, data) {
    if (!this.events[event]) return;
    this.events[event].forEach((callback) => callback(data));
  },
  subscribe: function (event, callback) {
    if (!this.events[event]) this.events[event] = [];
    this.events[event].push(callback);
  },
  isSubscribed: function (event) {
    if (!Array.isArray(this.events[event])) return false;
    else {
      if (this.events[event].length == 0) return false;
      else return true;
    }
  },
};
