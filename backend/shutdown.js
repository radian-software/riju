import { langWatcher } from "./langs.js";

export function shutdown() {
  langWatcher.close();
}
