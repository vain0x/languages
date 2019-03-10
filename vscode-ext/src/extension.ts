/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

// Entry point of the VSCode extension.

import * as path from "path"
import { ExtensionContext, workspace } from "vscode"
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient"

let client: LanguageClient

export function activate(context: ExtensionContext) {
  let serverPath = context.asAbsolutePath("../compiler/target/debug/lsp-server")
  let serverOptions: ServerOptions = {
    command: serverPath,
  }

  let clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file" },
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

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop()
  }
}
