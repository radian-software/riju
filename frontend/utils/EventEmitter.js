export const EventEmitter = {
  events: {},
  lastUid: -1,
  dispatch: function (event, data) {
    if (!this.events[event]) return;
    for (let token of Object.keys(this.events[event])) {
      const callback = this.events[event][token];
      if (callback && typeof callback == "function") callback(data);
    }
  },
  subscribe: function (event, callback) {
    if (!callback) return;
    if (!this.events[event]) this.events[event] = {};
    const token = "uID_" + String(++this.lastUid);
    this.events[event][token] = callback;
  },
  unsubcribe: function (...tokens) {
    for (let k of Object.keys(events)) {
      let toks = Object.keys(events[k]);
      for (let tok of toks) {
        if (tokens.includes(tok)) delete events[k][tok];
      }
    }
  },
};
