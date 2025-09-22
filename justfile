#!/usr/bin/env -S just --justfile

alias f := fmt
alias l := lint
alias c := comply
alias k := check

[doc('List available commands')]
_default:
    just --list --unsorted

[doc('Setup the project')]
setup:
    cp -n _scripts/hooks/pre-commit .git/hooks/
    # emacs, jinx
    sudo apt install --assume-yes libenchant-2-dev markdown
    # lsp
    cargo binstall emacs-lsp-booster
    # npm install -g yaml-language-server

[doc('Tasks to make the code-base comply with the rules. Mostly used in git hooks')]
comply: fmt lint

[doc('Check if the repository comply with the rules and ready to be pushed')]
check: fmt-check lint

[doc('Format the codebase')]
fmt:
    dprint fmt

[doc('Check is the codebase properly formatted')]
fmt-check:
    dprint check

[doc('Lint the codebase')]
lint:
    typos

[doc('Create a new release. Example `cargo-release release minor --tag-name v0.2.0`')]
release version:
    ./release {{ version }}
