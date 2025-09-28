# SKALP Language Support for VS Code

This extension provides language support for SKALP hardware description language in Visual Studio Code.

## Features

- **Syntax Highlighting**: Full syntax highlighting for SKALP files (.sk, .skalp)
- **Auto-completion**: Context-aware code completion for:
  - Keywords (entity, impl, protocol, trait, etc.)
  - Types (bit, logic, int, nat, clock, reset)
  - Events (clock.rise, reset.active, etc.)
- **Hover Information**: Detailed documentation on hover
- **Go to Definition**: Navigate to symbol definitions
- **Find References**: Find all references to a symbol
- **Diagnostics**: Real-time error and warning detection
- **Document Symbols**: Outline view support

## Requirements

- The SKALP language server (`skalp-lsp`) must be installed and available in your PATH
- VS Code 1.74.0 or higher

## Installation

### From Source

1. Build the SKALP language server:
   ```bash
   cd crates/skalp-lsp
   cargo build --release
   ```

2. Add the language server to your PATH:
   ```bash
   export PATH=$PATH:/path/to/skalp/target/release
   ```

3. Install the VS Code extension:
   ```bash
   cd vscode-skalp
   npm install
   npm run compile
   ```

4. Open VS Code and run the extension in development mode (F5)

## Extension Settings

This extension contributes the following settings:

* `skalp.serverPath`: Path to the SKALP language server executable (default: `skalp-lsp`)
* `skalp.trace.server`: Trace server communication for debugging

## Known Issues

- Initial release, please report issues on GitHub

## Release Notes

### 0.1.0

Initial release with basic language support:
- Syntax highlighting
- LSP integration
- Basic IDE features