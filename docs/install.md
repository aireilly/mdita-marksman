---
author: Aidan Reilly
keyword: [installation, setup]
---

# Installation

Pre-built self-contained binaries are available from [GitHub Releases](https://github.com/aireilly/mdita-marksman/releases). The current version is **1.0.7**.

1. Download the binary for your platform from the [latest release](https://github.com/aireilly/mdita-marksman/releases/latest):

   | Platform | Binary |
   |----------|--------|
   | Linux x64 | `mdita-marksman-linux-x64` |
   | Linux arm64 | `mdita-marksman-linux-arm64` |
   | macOS Apple Silicon | `mdita-marksman-macos-arm64` |
   | macOS Intel | `mdita-marksman-macos-x64` |
   | Windows x64 | `mdita-marksman-win-x64.exe` |

2. Make the binary executable (Linux/macOS):
   ```bash
   chmod +x mdita-marksman-linux-x64
   ```

3. Copy it to a directory on your `PATH` (Linux):
   ```bash
   cp mdita-marksman-linux-x64 $HOME/.local/bin/mdita-marksman
   ```

No runtime dependencies are required -- the binary is self-contained.
