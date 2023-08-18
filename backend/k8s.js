import * as k8sClient from "@kubernetes/client-node";
import fetch from "node-fetch";
import WebSocket from "ws";

const kubeconfig = new k8sClient.KubeConfig();
kubeconfig.loadFromDefault();

const k8s = kubeconfig.makeApiClient(k8sClient.CoreV1Api);

export function watchPods() {
  const callbacks = {};
  const pods = {};

  // https://github.com/kubernetes-client/javascript/blob/1f76ee10c54e33a998abb4686488ccff4285366a/examples/typescript/informer/informer.ts
  //
  // The watch functionality seems to be wholly undocumented. Copy,
  // paste, and pray.
  const informer = k8sClient.makeInformer(
    kubeconfig,
    "/api/v1/namespaces/user/pods",
    () => k8s.listNamespacedPod("user")
  );

  for (const event of ["add", "update", "delete"]) {
    informer.on(event, (pod) => {
      if (pod.metadata.name in callbacks) {
        callbacks[pod.metadata.name](event, pod);
      }
      pods[pod.metadata.name] = pod;
      if (event === "delete") {
        delete callbacks[pod.metadata.name];
        delete pods[pod.metadata.name];
      }
    });
  }

  informer.on("error", (err) => {
    console.error(err);
    setTimeout(() => informer.start(), 5000);
  });
  informer.start();

  return {
    setCallback: (podName, callback) => {
      callbacks[podName] = callback;
      if (podName in pods) {
        callback("add", pods[podName]);
      }
    },
    close: () => {
      informer.stop();
    },
  };
}

export function watchConfigMaps() {
  const callbacks = {};
  const configMaps = {};

  const informer = k8sClient.makeInformer(
    kubeconfig,
    "/api/v1/namespaces/database",
    () => k8s.listNamespacedConfigMap("database")
  );

  for (const event of ["add", "update", "delete"]) {
    informer.event(event, (configMap) => {
      if (event === "delete") {
        // Ignore this explicitly
        return;
      }
      if (configMap.metadata.name in callbacks) {
        callbacks[configMap.metadata.name](event, configMap);
      }
      configMaps[configMaps.metadata.name] = configMap;
    });
  }

  informer.on("error", (err) => {
    console.error(err);
    setTimeout(() => informer.start(), 5000);
  });
  informer.start();

  return {
    setCallback: (configMapName, callback) => {
      callbacks[configMapName] = callback;
      if (configMapName in configMaps) {
        callback("add", configMaps[configMapName]);
      }
    },
    close: () => {
      informer.stop();
    },
  };
}

export async function listUserSessions() {
  return (await k8s.listNamespacedPod("user")).body.items.map((pod) => ({
    podName: pod.metadata.name,
    sessionID: pod.metadata.labels["riju.codes/user-session-id"],
  }));
}

export async function createUserSession({
  sessionID,
  containerHostname,
  langImageTag,
}) {
  const pod = (
    await k8s.createNamespacedPod("user", {
      metadata: {
        name: `riju-user-session-${sessionID}`,
        labels: {
          "riju.codes/user-session-id": sessionID,
        },
      },
      spec: {
        hostname: containerHostname,
        imagePullSecrets: [
          {
            name: "registry-user-login",
          },
        ],
        containers: [
          {
            name: "session",
            image: `localhost:30999/riju-lang:${langImageTag}`,
            resources: {
              requests: {},
              limits: {
                cpu: "1000m",
                memory: "4Gi",
              },
            },
            env: [
              // For agent
              {
                name: "RIJU_AGENT_COMMAND_PREFIX",
                value: "runuser -u riju --",
              },
              // For user code
              {
                name: "HOME",
                value: "/home/riju",
              },
              {
                name: "LANG",
                value: "C.UTF-8",
              },
              {
                name: "LC_ALL",
                value: "C.UTF-8",
              },
              {
                name: "LOGNAME",
                value: "riju",
              },
              {
                name: "PATH",
                value:
                  "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
              },
              {
                name: "PWD",
                value: "/home/riju/src",
              },
              {
                name: "SHELL",
                value: "/usr/bin/bash",
              },
              {
                name: "TERM",
                value: "xterm-256color",
              },
              {
                name: "TMPDIR",
                value: "/tmp",
              },
              {
                name: "USER",
                value: "riju",
              },
              {
                name: "USERNAME",
                value: "riju",
              },
            ],
            workingDir: "/home/riju/src",
            securityContext: {
              runAsUser: 0,
            },
            startupProbe: {
              httpGet: {
                path: "/health",
                port: 869,
                scheme: "HTTP",
              },
              failureThreshold: 30,
              initialDelaySeconds: 0,
              periodSeconds: 1,
              successThreshold: 1,
              timeoutSeconds: 2,
            },
            readinessProbe: {
              httpGet: {
                path: "/health",
                port: 869,
                scheme: "HTTP",
              },
              failureThreshold: 1,
              initialDelaySeconds: 2,
              periodSeconds: 10,
              successThreshold: 1,
              timeoutSeconds: 2,
            },
            livenessProbe: {
              httpGet: {
                path: "/health",
                port: 869,
                scheme: "HTTP",
              },
              failureThreshold: 3,
              initialDelaySeconds: 2,
              periodSeconds: 10,
              successThreshold: 1,
              timeoutSeconds: 2,
            },
            volumeMounts: [
              {
                name: "riju-bin",
                mountPath: "/riju-bin",
                readOnly: true,
              },
            ],
          },
        ],
        restartPolicy: "Never",
      },
    })
  ).body;
  return pod.metadata.name;
}

export async function initUserSession({ watcher, podName, proxyInfo }) {
  let done = false;
  try {
    return await new Promise(async (resolve, reject) => {
      let timeout = null;
      try {
        timeout = setTimeout(
          () => reject("timed out waiting for pod to become ready"),
          5 * 60 * 1000
        );
        const podIP = await new Promise((resolve, reject) => {
          watcher.setCallback(podName, (event, pod) => {
            if (event == "delete") {
              reject(new Error("pod was deleted"));
            } else if (pod.status.phase === "Failed") {
              reject(new Error("pod status became Failed"));
            } else if (pod.status.podIP) {
              resolve(pod.status.podIP);
            }
          });
        });
        while (!done) {
          let resp;
          try {
            if (!proxyInfo) {
              resp = await fetch(`http://${podIP}:869`);
            } else {
              resp = await fetch(
                `${proxyInfo.httpProtocol}://${proxyInfo.host}:${proxyInfo.port}/${podIP}/health`,
                {
                  headers: {
                    Authorization: `Basic ${Buffer.from(
                      `${proxyInfo.username}:${proxyInfo.password}`
                    ).toString("base64")}`,
                  },
                }
              );
            }
            if (!resp.ok) {
              throw new Error(`Got HTTP error ${resp.status}`);
            } else {
              done = true;
            }
          } catch (err) {
            await new Promise((resolve) => setTimeout(resolve, 250));
          }
        }
        resolve({
          exec: async (cmdline, { on, pty }) => {
            // on :: { stdout, stderr, exit, error, close }
            if (pty) {
              cmdline = ["/riju-bin/ptyify", ...cmdline];
            }
            const params = new URLSearchParams();
            for (const arg of cmdline) {
              params.append("cmdline", arg);
            }
            let conn;
            if (!proxyInfo) {
              conn = new WebSocket(
                `ws://${podIP}:869/exec?${params.toString()}`
              );
            } else {
              conn = new WebSocket(
                `${proxyInfo.wsProtocol}://${proxyInfo.host}:${
                  proxyInfo.port
                }/${podIP}/exec?${params.toString()}`,
                {
                  headers: {
                    Authorization: `Basic ${Buffer.from(
                      `${proxyInfo.username}:${proxyInfo.password}`
                    ).toString("base64")}`,
                  },
                }
              );
            }
            await new Promise((resolve, reject) => {
              conn.on("open", resolve);
              conn.on("error", reject);
            });
            conn.on("message", (msg) => {
              let event, data, text, exitStatus;
              try {
                ({ event, data, text, exitStatus } = JSON.parse(msg));
              } catch (err) {
                on.error(
                  `Unable to parse JSON message from agent: ${JSON.stringify(
                    msg
                  )}`
                );
              }
              switch (event) {
                case "stdout":
                  on.stdout(Buffer.from(data, "base64"));
                  break;
                case "stderr":
                  on.stderr(Buffer.from(data, "base64"));
                  break;
                case "exit":
                  on.exit(exitStatus);
                  break;
                case "warn":
                case "error":
                  on.error(text);
                  break;
                default:
                  on.error(`Unexpected event type: ${JSON.stringify(event)}`);
                  break;
              }
            });
            conn.on("close", () => {
              on.close();
            });
            conn.on("error", (err) => {
              on.error(`Websocket closed with error: ${err}`);
              on.close();
            });
            return {
              stdin: {
                // data should be of type Buffer
                write: (data) =>
                  conn.send(
                    JSON.stringify({
                      event: "stdin",
                      data: data.toString("base64"),
                    })
                  ),
              },
              close: () => {
                conn.close();
              },
            };
          },
        });
      } catch (err) {
        reject(err);
      } finally {
        if (timeout) {
          clearTimeout(timeout);
        }
      }
    });
  } finally {
    done = true;
  }
}

export async function deleteUserSessions(sessionsToDelete) {
  for (const { podName } of sessionsToDelete) {
    await k8s.deleteNamespacedPod(podName, "user");
  }
}
