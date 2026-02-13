# MDITA Marksman

MDITA Marksman is a language server for MDITA (Markdown DITA) documents. It integrates with your editor to assist you in writing and maintaining MDITA content, providing code assist and intelligence.

<img width="650" alt="image" src="https://github.com/user-attachments/assets/12c41c9a-60a2-4e7f-a034-197e573a66db" />


Based on the [Marksman](https://github.com/artempyanykh/marksman) LSP server, MDITA Marksman extends it with support for the [MDITA syntax specification](https://www.dita-ot.org/dev/reference/markdown/mdita-syntax) from DITA-OT.

Using the [LSP protocol][lsp-main] it provides **completion**, goto **definition**, find **references**, **rename**
refactoring, **diagnostics**, and more. It supports both standard Markdown links and **wiki-link**-style references.

MDITA Marksman is built for **Linux** and is distributed as a **self-contained binary**.

The server provides assistance with:

* MDITA YAML front matter completion (author, source, publisher, permissions, audience, category, keyword, resourceid)
* MDITA-specific diagnostics (missing front matter, missing short description, heading hierarchy violations)
* MDITA map file (`.mditamap`) support
* Markdown inline links:
   ```md
   This is [inline link](/some-file.md#some-heading).
   This is an internal [anchor link](#heading).
   ```
* Markdown reference links:
   ```md
   See [reference][ref-label].

   [ref-label]: /url "Title"
   ```
* Wiki-links:
   ```md
   Link to [[another-note]].
   Link to [[another-notes#heading]].
   Internal link to [[#a-heading]].
   ```

All types of links support completion, hover, goto definition/references. Additionally, MDITA Marksman provides diagnostics
for wiki-links to detect broken references and duplicate/ambiguous headings.

## Changes from upstream Marksman

The following table summarizes the changes in MDITA Marksman compared to the upstream [Marksman](https://github.com/artempyanykh/marksman) project.

| Area | Change | Details |
|------|--------|---------|
| MDITA YAML front matter | Added | New `YamlMetadata` type with completion for MDITA fields: `author`, `source`, `publisher`, `permissions`, `audience`, `category`, `keyword`, `resourceid`. |
| MDITA map files | Added | New `.mditamap` file format for defining topic hierarchies using nested Markdown lists of links. |
| DITA fragment identifiers | Added | Support for DITA-style `topicID/sectionID` fragment syntax in links. |
| MDITA diagnostics | Added | Warnings for missing YAML front matter, missing short description after H1, skipped heading levels, Extended Profile feature gating, footnote validation, and admonition type validation. Enabled by default; set `core.mdita.enable = false` to disable. |
| MDITA keyref detection | Added | Shortcut reference links (`[key]`) produce warnings for unresolved keyrefs in MDITA mode. |
| Short description tracking | Added | Index tracks the first paragraph after H1 as the document short description for MDITA compliance. |
| Configuration file | Renamed | `.marksman.toml` renamed to `.mdita-marksman.toml`. User config directory changed to `~/.config/mdita-marksman/`. |
| Configuration options | Added | New `[core.mdita]` section with `enable` (bool) and `map_extensions` (string array) settings. |
| Default file extensions | Changed | `core.markdown.file_extensions` default now includes `mditamap` alongside `md` and `markdown`. |
| Hashtag/tag support | Removed | All `#hashtag` features (parsing, completion, semantic tokens, find references) removed. |
| Platform support | Narrowed | Build targets reduced to Linux only (x64, arm64). Windows, macOS, and FreeBSD support removed. |
| Benchmarks | Removed | `Benchmarks/` project removed from the solution. |
| Diagnostic source label | Changed | Diagnostic `Source` field changed from `Marksman` to `MDITA Marksman`. |
| .NET SDK | Changed | Target SDK updated to .NET 10.0 preview. |
| CI/CD | Changed | Simplified to Linux-only workflows. Added `ci.yml` and `version-bump.yml`, removed `build.yml`. |

## MDITA diagnostics

MDITA mode is **enabled by default**. To disable it (plain Markdown LSP behavior), add to your `.mdita-marksman.toml`:

```toml
[core.mdita]
enable = false
```

### Schema-aware validation

Set `$schema` in your YAML front matter to enable schema-specific checks:

| Schema | Diagnostic |
|--------|-----------|
| `urn:oasis:names:tc:dita:xsd:task.xsd` | Warns if no ordered list (procedure steps) is present |
| `urn:oasis:names:tc:dita:xsd:concept.xsd` | Info if ordered list found (may belong in a task topic) |
| `urn:oasis:names:tc:dita:xsd:reference.xsd` | Info if no table or definition list is present |
| `urn:oasis:names:tc:dita:xsd:map.xsd` | Info if body content beyond navigation links is found |

### Extended Profile gating

Documents using the Core Profile (`urn:oasis:names:tc:mdita:core:xsd:topic.xsd`) will produce warnings when Extended-only features are used:

- Definition lists
- Footnotes (`[^label]`)
- Strikethrough (`~~text~~`)
- Generic attributes (`{#id .class}`)
- Admonitions (`!!! type`)

Documents using the Extended Profile or DITA schemas are not gated.

### Footnote validation

Across all profiles, the linter detects:

- Footnote references (`[^label]`) with no matching definition
- Footnote definitions (`[^label]: text`) that are never referenced

### Admonition validation

Admonition blocks (`!!! type`) are validated against DITA note types: `note`, `tip`, `warning`, `caution`, `danger`, `attention`, `important`, `notice`, `fastpath`, `remember`, `restriction`, `trouble`.

## Configuration

MDITA Marksman uses `.mdita-marksman.toml` as its project-level configuration file. Place it at the root of your project.

User-level configuration is stored in `~/.config/mdita-marksman/config.toml`.

See the [configuration page](/docs/configuration.md) for details.

## How to install

See the [installation instructions](/docs/install.md).

## Features

See [the Features page](/docs/features.md) to learn more about language features, configurations, and single- and multi-file modes.

## FAQ

* Cross-file references and completions don't work.
    + Either create an empty `.mdita-marksman.toml` in the root folder of your project or initialize a repository (e.g. `git init`). See [the Features page](/docs/features.md) to learn more about single- and multi-file modes.

[lsp-main]: https://microsoft.github.io/language-server-protocol/
