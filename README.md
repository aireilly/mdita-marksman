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
