import crypto from "crypto";
import { promises as fs } from "fs";
import process from "process";
import url from "url";

import { Command } from "commander";
import _ from "lodash";
import { v4 as getUUID } from "uuid";

import { getLangs, getPackages, readLangConfig } from "../lib/yaml.js";
import {
  getLocalImageDigest,
  getLocalImageLabel,
  getRemoteImageLabel,
  getDockerRepo,
} from "./docker-util.js";
import { hashDockerfile } from "./hash-dockerfile.js";
import { runCommand } from "./util.js";

function printTable(data, headers) {
  const widths = headers.map(({ key, title }) =>
    Math.max(title.length, ...data.map((datum) => datum[key].length))
  );
  [
    headers.map(({ title }) => title.toUpperCase()),
    widths.map((width) => "-".repeat(width)),
    ...data.map((datum) => headers.map(({ key }) => datum[key])),
  ].map((values) =>
    console.log(
      values.map((value, idx) => value.padEnd(widths[idx])).join("  ")
    )
  );
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const program = new Command();
  program.option("--execute", "pull and build artifacts locally");
  program.option(
    "--publish",
    "with --execute, also deploy newly built artifacts"
  );
  program.option("--show-all", "show also unchanged artifacts");
  program.option(
    "--omit-unneeded-downloads",
    "don't download artifacts unless needed for dependent builds"
  );
  program.parse(process.argv);
  let plan = await computePlan();
  let tableData = plan.map(
    ({
      id,
      deps,
      artifact,
      name,
      desired,
      local,
      remote,
      download,
      build,
      upload,
    }) => {
      let action, details, func, couldPrune, noop;
      if (remote === desired && local === desired) {
        action = "(no action)";
        details = desired;
        func = () => {};
        couldPrune = true;
        noop = true;
      } else if (remote === desired && local !== desired) {
        action = "download remote";
        details = `${local} => ${desired}`;
        func = download;
        couldPrune = true;
        noop = false;
      } else if (local === desired && remote !== desired) {
        action = "publish local";
        details = `${remote} => ${desired}`;
        func = async () => {
          if (program.publish) {
            await upload();
          }
        };
        couldPrune = false;
        noop = false;
      } else {
        action = "rebuild and publish";
        if (local === remote) {
          details = `${local} => ${desired}`;
        } else {
          details = `${local} (local), ${remote} (remote) => ${desired}`;
        }
        func = async () => {
          await build();
          if (program.publish) {
            await upload();
          }
        };
        couldPrune = false;
        noop = false;
      }
      return {
        id,
        deps,
        couldPrune,
        artifact,
        name,
        action,
        details,
        func,
        noop,
      };
    }
  );
  if (program.omitUnneededDownloads) {
    for (const datum of [...tableData].reverse()) {
      if (
        datum.couldPrune &&
        _.every(
          tableData,
          (otherDatum) =>
            otherDatum.couldPrune || !otherDatum.deps.includes(datum.id)
        )
      ) {
        datum.pruned = true;
        if (!datum.noop) {
          datum.action += " [skipping]";
          datum.noop = true;
        }
      }
    }
  }
  console.log();
  const filteredTableData = tableData.filter(({ noop }) => !noop);
  if (filteredTableData.length === 0) {
    console.log(`*** NO ACTION REQUIRED FOR ${plan.length} ARTIFACTS ***`);
  } else {
    console.log(
      `*** ACTION REQUIRED FOR ${filteredTableData.length} of ${plan.length} ARTIFACTS ***`
    );
  }
  console.log();
  if (!program.showAll) {
    tableData = filteredTableData;
  }
  if (tableData.length > 0) {
    printTable(tableData, [
      { key: "artifact", title: "Type" },
      { key: "name", title: "Name" },
      { key: "action", title: "Action" },
      { key: "details", title: "Details" },
    ]);
    console.log();
  }
  tableData = filteredTableData;
  if (program.execute) {
    for (const { func } of tableData) {
      await func();
    }
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
