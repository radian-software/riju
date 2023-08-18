import * as k8s from "./k8s.js";

export class SessionManager {
  constructor() {
    this.podWatcher = k8s.watchPods();
    this.configMapWatcher = k8s.watchConfigMaps();
  }

  getSession({ sessionID, langID }) {
    const session = k8s.createUserSession({ sessionID });
  }
}

export class SessionBase {
  //
}
