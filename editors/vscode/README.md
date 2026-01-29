# Ceca VS Code Extension

This extension wires VS Code to the Ceca LSP server implemented in `ceca`.

## Setup

1. Build the Ceca executable so it is available in your `PATH`:

   ```bash
   cabal build
   ```

   Or install it locally so `ceca` resolves on your path:

   ```bash
   cabal install ceca
   ```

2. Install the extension dependencies:

   ```bash
   cd editors/vscode
   npm install
   ```

3. Open this repo in VS Code and run the extension in the Extension Development Host.

## Settings

- `ceca.serverPath`: Path to the `ceca` executable (default: `ceca`).
- `ceca.serverArgs`: Arguments passed to the LSP server (default: `--mode lsp`).

## Notes

The Ceca LSP server currently only provides completion for record fields. If you change the server path or args, the extension restarts the client automatically.
