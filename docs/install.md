---
author: Aidan Reilly
keyword: [installation, setup, build]
---

# Installation

MDITA Marksman is built from source on Linux.

## Prerequisites

Install the [.NET SDK](https://dotnet.microsoft.com/en-us/download) (version 10.0 or later).

On Fedora:

```bash
sudo dnf install dotnet-sdk-10.0
```

## Build from source

1. Clone the repository:
   ```bash
   git clone <repo-url> mdita-marksman
   ```
2. Inside the `mdita-marksman` folder run:
   ```bash
   make install
   ```
3. The binary will be installed under `$HOME/.local/bin` (make sure this folder is in your `PATH`).
