---
author: Aidan Reilly
keyword: [features, completion, diagnostics, wiki-links, code-actions, mdita]
---

# Features

MDITA Marksman provides a comprehensive set of language features for MDITA (Markdown DITA) editing, including completion, navigation, diagnostics, and refactoring.

- (Done) Document symbols from headings.
- (Done) Workspace symbols from headings.
    * Query matching is subsequence-based, that is `lsp` will match both `LSP` and `Low Seismic Profile`.
- (Done) Completion for links (inline, reference, wiki).
- (Done) Hover preview for links.
- (Done) "Go to definition" for links.
- (Done) "Find references" for headings and links.
- (Done) Diagnostics for wiki-links.
- (Done) Support multi-folder workspaces.
- (Done) Custom parser for more fine-grained note structure.
- (Done) Code Lens with "# references" on headings.
- (Done) Rename refactor.
- (Done) MDITA YAML front matter completion.
- (Done) MDITA-specific diagnostics (missing front matter, short description, heading hierarchy).
- (Done) MDITA map file (`.mditamap`) support.
- (Done) DITA fragment identifier support (`topicID/sectionID`).
- (Done) Definition list support.

## Configuration

See [Configuration](/docs/configuration.md) docs for more details.

## Wiki links

Alongside regular markdown links, MDITA Marksman also supports wiki-style links, e.g. ``[[some-doc]]``
or ``[[#some-heading]]``.

Documents can be referred either by their **title** or **filename**.
The exact behavior depends on the combination of settings.

### Titles from headings

Level 1 heading (`# Foo`) is treated as document's title.
This implies that there supposed to be at most one 'level 1 heading' in a document.

This is the default behavior. Users that don't want this behavior can change it in their
`.mdita-marksman.toml`:

```toml
[core]
title_from_heading = false
```

Note that setting `core.title_from_heading` to `false` automatically changes the default completion style to a
file-based one.
See [this config file](../Tests/default.mdita-marksman.toml) for more details.

### Completion style for wiki link

Wiki links can use both a title and a filename to reference a document.
The preferred style of completion is configured via `completion.wiki.style` configuration setting.

See [the example config file](../Tests/default.mdita-marksman.toml) for more details.

## Code actions

Code actions usually can be enabled/disabled via a configuration option. See
[configuration](#configuration) for more details.

### Table of Contents

MDITA Marksman has a code action to create and update a table of contents of a document.

## Ignore files

MDITA Marksman by default reads ignore globs from `.gitignore`, `.hgignore`, and
`.ignore` and doesn't scan directories matching any of the glob patterns.

## Workspace folders, project roots, and single-file mode

The LSP specification is designed to work with projects rather than individual
files. MDITA Marksman has a custom **single-file mode** that provides a *subset*
of language features for markdown files open outside of any project.

How a folder (aka project, aka root) is found varies between editors, but
usually it's either

1. a root of a VCS repository (applicable to all languages),
2. a folder with `.mdita-marksman.toml` marker file.

When MDITA Marksman doesn't provide cross-file language assist for your files and you
don't understand why, you can either:

1. check your project into version control, or
2. create a `.mdita-marksman.toml` at the root folder of your project, or
3. refer to your editor/LSP client documentation regarding how a project root
   is defined.
