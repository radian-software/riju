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
    "/api/v1/namespaces/riju-user/pods",
    () => k8s.listNamespacedPod("riju-user")
  );

  for (const event of ["add", "update", "delete"]) {
    informer.on(event, (pod) => {
      if (pod.metadata.name in callbacks) {
        callbacks[pod.metadata.name](event, pod);
      }
      pods[pod.metadata.name] = pod;
      if (event == "delete") {
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
    podExists: (podName) => {
      return podName in pods;
    },
  };
}

export async function listUserSessions() {
  return (await k8s.listNamespacedPod("riju-user")).body.items.map((pod) => ({
    podName: pod.metadata.name,
    sessionID: pod.metadata.labels["riju.codes/user-session-id"],
  }));
}

export async function createUserSession({
  watcher,
  sessionID,
  langConfig,
  revisions,
}) {
  const pod = (
    await k8s.createNamespacedPod("riju-user", {
      metadata: {
        name: `riju-user-session-${sessionID}`,
        labels: {
          "riju.codes/user-session-id": sessionID,
        },
      },
      spec: {
        volumes: [
          {
            name: "minio-config",
            secret: {
              secretName: "minio-user-login",
            },
          },
          {
            name: "riju-bin",
            emptyDir: {},
          },
        ],
        imagePullSecrets: [
          {
            name: "registry-user-login",
          },
        ],
        initContainers: [
          {
            name: "download",
            image: "minio/mc:RELEASE.2022-12-13T00-23-28Z",
            resources: {},
            command: ["sh", "-c"],
            args: [
              `mkdir -p /root/.mc && cp -LT /mc/config.json /root/.mc/config.json &&` +
                `mc cp riju/agent/${revisions.agent} /riju-bin/agent && chmod +x /riju-bin/agent &&` +
                `mc cp riju/ptyify/${revisions.ptyify} /riju-bin/ptyify && chmod +x /riju-bin/ptyify`,
            ],
            volumeMounts: [
              {
                name: "minio-config",
                mountPath: "/mc",
                readOnly: true,
              },
              {
                name: "riju-bin",
                mountPath: "/riju-bin",
              },
            ],
          },
        ],
        containers: [
          {
            name: "session",
            image: `localhost:30999/riju-lang:${langConfig.id}-${revisions.langImage}`,
            resources: {
              requests: {},
              limits: {
                cpu: "1000m",
                memory: "4Gi",
              },
            },
            command: ["/riju-bin/agent"],
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
      try {
        setTimeout(
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
          exec: (cmdline, { on, pty }) => {
            // on :: { stdout, stderr, exit, error, close }
            if (pty) {
              cmdline = ["/riju-bin/ptyify", ...cmdline];
            }
            let conn;
            if (!proxyInfo) {
              conn = new WebSocket(
                `ws://${podIP}:869/exec?${new URLSearchParams({
                  cmdline,
                }).toString()}`
              );
            } else {
              conn = new WebSocket(
                `${proxyInfo.wsProtocol}://${proxyInfo.host}:${
                  proxyInfo.port
                }/${podIP}/exec?${new URLSearchParams({
                  cmdline,
                }).toString()}`,
                {
                  headers: {
                    Authorization: `Basic ${Buffer.from(
                      `${proxyInfo.username}:${proxyInfo.password}`
                    ).toString("base64")}`,
                  },
                }
              );
            }
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
                  on.stdout(data);
                  break;
                case "stderr":
                  on.stderr(data);
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
                write: (data) =>
                  conn.write(JSON.stringify({ event: "stdin", data })),
              },
            };
          },
        });
      } catch (err) {
        reject(err);
      }
    });
  } finally {
    done = true;
  }
}

export async function deleteUserSessions(sessionsToDelete) {
  for (const { podName } of sessionsToDelete) {
    await k8s.deleteNamespacedPod(podName, "riju-user");
  }
}
