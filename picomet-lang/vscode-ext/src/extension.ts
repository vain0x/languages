/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

// Entry point of the VSCode extension.

import { ExtensionContext, workspace, commands } from "vscode"
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient"

let client: LanguageClient

const getPicometBin = (context: ExtensionContext) => {
  const config = workspace.getConfiguration("picomet-lang")

  const relativePath = process.env.PICOMET_BIN
    || config.get("picomet-bin") as string | undefined
    || "./out/picomet"

  return context.asAbsolutePath(relativePath)
}

const startLspClient = (context: ExtensionContext) => {
  const picometBinFullPath = getPicometBin(context)
  let serverOptions: ServerOptions = {
    command: picometBinFullPath,
    args: ["lsp"],
  }

  let clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "picomet-lang" },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  }

  // Start language server and client.
  client = new LanguageClient(
    "picomet-lang",
    "Picomet-lang LSP",
    serverOptions,
    clientOptions
  )
  client.start()
}

export function activate(context: ExtensionContext) {
  commands.registerCommand("getPicometBin", () => getPicometBin(context))

  startLspClient(context)
}

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop()
  }
}
