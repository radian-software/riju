import process from "process";

import semaphore from "semaphore";

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
    return null;
  }
  const labels = JSON.parse(output)[0].Config.Labels;
  return (labels && labels[label]) || null;
}

// Return the list of tags in a remote Docker repository.
export async function getRemoteRepositoryTags(repo) {
  return JSON.parse(
    (
      await runCommand(`skopeo list-tags "docker://${repo}"`, {
        getStdout: true,
      })
    ).stdout
  ).Tags;
}

const remoteImageRateLimiter = semaphore(16);

// Return the value of a label on a Docker image that is on a remote
// registry. If the image or label doesn't exist, return null. You
// have to pass in a list of tags on the remote repository (see
// getRemoteRepositoryTags) so that we can distinguish between missing
// images and network errors.
export async function getRemoteImageLabel(image, label, tags) {
  await new Promise((resolve) => remoteImageRateLimiter.take(resolve));
  try {
    const [_repo, tag] = image.split(":");
    let output;
    try {
      output = (
        await runCommand(`skopeo inspect docker://${image}`, {
          getStdout: true,
        })
      ).stdout;
    } catch (err) {
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
    return (labels && labels[label]) || null;
  } finally {
    remoteImageRateLimiter.leave();
  }
}

// Return the value of $DOCKER_REPO, throwing an error if it's not set
// in the environment.
export function getDockerRepo() {
  if (!process.env.DOCKER_REPO) {
    throw new Error(`unset environment variable: \$DOCKER_REPO`);
  }
  return process.env.DOCKER_REPO;
}
