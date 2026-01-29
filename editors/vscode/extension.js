"use strict";

const vscode = require("vscode");
const cp = require("child_process");
const { LanguageClient } = require("vscode-languageclient/node");

let client;
let outputChannel;

function normalizeServerArgs(value) {
  const args = Array.isArray(value) ? value.slice() : [];
  const sanitized = args.filter(
    (arg) => arg !== "--stdio" && arg !== "--node-ipc"
  );
  const hadRemoved = sanitized.length !== args.length;
  const effective = sanitized.length === 0 ? ["--mode", "lsp"] : sanitized;

  const hasModeFlag = effective.includes("--mode") || effective.includes("-m");
  if (!hasModeFlag) {
    return {
      args: ["--mode", "lsp", ...effective],
      removedUnsupported: hadRemoved,
    };
  }

  return { args: effective, removedUnsupported: hadRemoved };
}

function createServerOptions() {
  const config = vscode.workspace.getConfiguration("ceca");
  const serverPath = config.get("serverPath", "ceca");
  const normalized = normalizeServerArgs(
    config.get("serverArgs", ["--mode", "lsp"])
  );
  const serverArgs = normalized.args;

  if (outputChannel) {
    if (normalized.removedUnsupported) {
      outputChannel.appendLine(
        "Ignoring unsupported server args: --stdio/--node-ipc"
      );
    }
    outputChannel.appendLine(
      `Starting Ceca LSP: ${serverPath} ${serverArgs.join(" ")}`
    );
  }

  return () =>
    new Promise((resolve, reject) => {
      const child = cp.spawn(serverPath, serverArgs, {
        stdio: ["pipe", "pipe", "pipe"],
      });

      child.once("error", (err) => {
        reject(err);
      });

      child.once("spawn", () => {
        resolve({ process: child, detached: false });
      });
    });
}

function createClientOptions() {
  return {
    documentSelector: [{ scheme: "file", language: "ceca" }],
    outputChannel,
  };
}

function startClient(context) {
  const serverOptions = createServerOptions();
  const clientOptions = createClientOptions();

  client = new LanguageClient(
    "ceca",
    "Ceca Language Server",
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(client.start());
}

function restartClient(context) {
  if (!client) {
    startClient(context);
    return;
  }

  client
    .stop()
    .then(() => {
      startClient(context);
    })
    .catch((err) => {
      vscode.window.showErrorMessage(
        `Failed to restart Ceca LSP client: ${err.message}`
      );
    });
}

function activate(context) {
  outputChannel =
    outputChannel ?? vscode.window.createOutputChannel("Ceca Language Server");
  startClient(context);

  const configWatcher = vscode.workspace.onDidChangeConfiguration((event) => {
    if (event.affectsConfiguration("ceca")) {
      restartClient(context);
    }
  });

  context.subscriptions.push(configWatcher);
}

function deactivate() {
  if (!client) {
    return undefined;
  }

  return client.stop();
}

module.exports = {
  activate,
  deactivate,
};
