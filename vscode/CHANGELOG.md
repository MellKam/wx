# Changelog

All notable changes to the WX Language Support VS Code extension will be
documented in this file. For changes to the WX language and compiler
itself, see the [root changelog](../CHANGELOG.md).

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [0.1.0] - 2026-07-09

First published release.

### Added

- Syntax highlighting for `.wx` files (TextMate grammar).
- Diagnostics, completions, and formatting via the `wx-lsp` language
  server, bundled per-platform (Linux, macOS x64/arm64, Windows).
- Format-on-save enabled by default for WX files.
- `wx.formatter.indentSize` and `wx.formatter.maxWidth` settings.
- "WX: Restart Language Server" command.
