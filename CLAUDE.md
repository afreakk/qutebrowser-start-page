# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

qutebrowser-start-page is a Haskell CLI tool that generates a styled HTML start page for the Qutebrowser web browser. It reads qutebrowser's quickmarks, bookmarks, and history to create a responsive HTML page with Tailwind CSS styling.

## Build Commands

```bash
# Build with Nix (recommended)
nix-build

# Build with Cabal (development)
cabal build

# Generate the start page
qutebrowser-start-page > startpage.html
```

## Architecture

The project consists of:

1. **Haskell executable** (`app/Main.hs`) - Single-file application that:
   - Reads qutebrowser config from `~/.config/qutebrowser/` (quickmarks, bookmarks)
   - Queries SQLite history database at `~/.local/share/qutebrowser/history.sqlite`
   - Generates HTML using Blaze-html library
   - Outputs to stdout

2. **Nix build system**:
   - `default.nix` - Main entry point, creates wrapper script
   - `haskellExecutable.nix` - Compiles Haskell code
   - `tailwindCss.nix` - Generates CSS from Tailwind

3. **Tailwind CSS** (`tailwind.config.js`, `input.css`) - Styling with custom grid classes for 1-4 column layouts

## Key Data Flow

```
Qutebrowser configs + SQLite → Parse → Generate Blaze HTML → stdout
```

The output HTML displays four sections (if non-empty): Quickmarks, Bookmarks, Frequent Sites, History. Grid columns adjust dynamically based on which sections have content.

Quickmarks are grouped by prefix (text before first hyphen). Groups with 2+ items get a header and have their prefix stripped from display (e.g., "gmail-work" and "gmail-home" appear as "work" and "home" under "GMAIL" header). Singleton and ungrouped items appear at the bottom below a divider.

## IDE Setup

`hie.yaml` configures Haskell Language Server for the single executable component.

## Installation

Available via NUR: `https://nur.nix-community.org/repos/afreakk/`
