#!/usr/bin/env -S just --justfile

# List available commands.
_default:
    just --list --unsorted

# Tasks to make the code-base comply with the rules. Mostly used in git hooks.
comply: fmt lint

# Check if the repository comply with the rules and ready to be pushed.
check: fmt-check lint

# Format the codebase.
fmt:
    dprint fmt --config configs/dprint.json

# Check is the codebase properly formatted.
fmt-check:
    dprint check --config configs/dprint.json

# Lint the codebase.
lint:
    typos --config configs/typos.toml

# Create a new release. Example `just release v2.2.0`
release version:
    bash scripts/release.sh {{ version }}
