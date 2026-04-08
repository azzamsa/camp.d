#!/usr/bin/env -S just --justfile

[doc('List available commands')]
_default:
    just --list --unsorted

[doc('Setup the project')]
setup: _hooks
    # emacs, jinx
    sudo apt install --assume-yes libenchant-2-dev markdown
    # lsp
    cargo binstall emacs-lsp-booster
    # npm install -g yaml-language-server

_hooks:
    #!/usr/bin/env bash
    for hook in .hooks/*; do \
        ln -sf "../../$hook" ".git/hooks/$(basename $hook)"; \
    done

[doc('Exhaustive quality check')]
qqq: qa meta

[doc('Quality check')]
qq: qa

[doc('Quick quality check')]
qa: fmt-check lint

[doc('Fix before check')]
qc: fix qq

[doc('Enforce rules')]
fix: fmt lint

[doc('Format')]
fmt:
    dprint fmt

[doc('Check formatting')]
fmt-check:
    dprint check

[doc('Lint')]
lint:
    typos

[doc('Update meta files')]
meta:
    emacs -Q --batch --eval "(princ (emacs-version))" > emacs-version

[doc('Create a new release')]
release version:
    ./release {{ version }}
