# CLAUDE.md

## Project overview

MDITA Marksman is an LSP (Language Server Protocol) server for MDITA (Markdown DITA) documents. It is a fork of [Marksman](https://github.com/artempyanykh/marksman), extended with MDITA-specific features including YAML front matter completion, MDITA diagnostics, and `.mditamap` support.

- **Language:** F# (.NET 10.0)
- **Repository:** `git@github.com:aireilly/mdita-marksman.git`
- **Current version:** 1.0.7 (set in `Marksman/Marksman.fsproj` `<VersionPrefix>`)
- **Release:** GitHub Releases via tag push (e.g., `v1.0.7`). Builds self-contained binaries for linux-x64, linux-arm64, macos-arm64, macos-x64, and win-x64.

## Build and test

```bash
make build          # Build the project
make test           # Run tests
make publish        # Publish a self-contained binary for the current platform
make install        # Build and install to $HOME/.local/bin
make fmt            # Format F# code with Fantomas and XML with xmllint
make check          # Check F# formatting
make clean          # Clean build artifacts
```

Requires .NET SDK 10.0+ (`global.json` uses `rollForward: latestMajor` from 9.0.100).

## Project structure

```
Marksman/           # Main LSP server (F#)
Tests/              # xUnit test project (F#)
LanguageServerProtocol/  # LSP protocol library
MarkdigPatches/     # Patches to the Markdig Markdown parser (C#)
docs/               # User-facing documentation (install, features, configuration)
scripts/            # Build/release scripts
.github/workflows/  # CI (ci.yml), Release (release.yml), Version bump (version-bump.yml)
```

## Key files

- `Marksman/Marksman.fsproj` -- project file, version, dependencies
- `.mdita-marksman.toml` -- project-level LSP config (user config at `~/.config/mdita-marksman/config.toml`)
- `.editorconfig` -- F# formatting rules (Fantomas)
- `Makefile` -- build, test, publish, install targets

## Code style

- F# code is formatted with **Fantomas** (run `make fmt`; check with `make check`)
- Bracket style: Stroustrup for `Marksman/` and `Tests/`
- Max line length: 100 characters for F# files
- Line endings: LF only

## CI/CD

- **CI** (`ci.yml`): Runs on push to `main` and PRs. Builds and tests on `ubuntu-latest`.
- **Release** (`release.yml`): Triggered by tag push (`v*`). Builds self-contained binaries for 5 platform targets and creates a GitHub Release with auto-generated notes.
- **Version bump** (`version-bump.yml`): Bumps `<VersionPrefix>` in the `.fsproj` after a release.

## Conventions

- The binary is named `mdita-marksman` (set via `<AssemblyName>` in the `.fsproj`).
- Version is derived from git tags at build time; `<VersionPrefix>` is the fallback.
- Configuration file is `.mdita-marksman.toml` (not `.marksman.toml`).
