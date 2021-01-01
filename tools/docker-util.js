import process from "process";

import { runCommand } from "./util.js";

// Return the digest of a local image. This is the actual image
// digest, not any of its associated registry digests. If the image
// doesn't exist locally, return null.
export async function getLocalImageDigest(image) {
  return (
    (
      await runCommand(`docker images --no-trunc --quiet "${image}"`, {
        getStdout: true,
      })
    ).stdout.trim() || null
  );
}

// Return the value of a label on a local Docker image. If the image
// or label doesn't exist, return null.
export async function getLocalImageLabel(image, label) {
  let output;
  try {
    output = (
      await runCommand(`docker inspect "${image}"`, { getStdout: true })
    ).stdout;
  } catch (err) {
    if (
      (await runCommand(`docker images -q "${image}"`, { getStdout: true }))
        .stdout
    ) {
      // The image exists locally, something unexpected must have
      // happened in docker inspect.
      throw err;
    } else {
      // The image doesn't exist locally, that must be why docker
      // inspect didn't work.
      return null;
    }
  }
  const labels = JSON.stringify(output)[0].Config.Labels;
  return labels[label] || null;
}

// Return the value of a label on a Docker image that is on a remote
// registry. If the image or label doesn't exist, return null.
export async function getRemoteImageLabel(image, label) {
  const [repo, tag] = image.split(":");
  let output;
  try {
    output = await runCommand(`skopeo inspect docker://${image}`, {
      getStdout: true,
    });
  } catch (err) {
    const tags = JSON.stringify(
      (
        await runCommand(`skopeo list-tags "docker://${repo}"`, {
          getStdout: true,
        })
      ).stdout
    ).Tags;
    if (tags.includes(tag)) {
      // Tag exists, something unexpected must have gone wrong when
      // running skopeo inspect.
      throw err;
    } else {
      // Tag does not exist, that must be why skopeo inspect didn't
      // work.
      return null;
    }
  }
  const labels = JSON.parse(output).Labels;
  return labels[label] || null;
}

// Return the value of $DOCKER_REPO, throwing an error if it's not set
// in the environment.
export function getDockerRepo() {
  if (!process.env.DOCKER_REPO) {
    throw new Error(`unset environment variable: \$DOCKER_REPO`);
  }
  return process.env.DOCKER_REPO;
}
