#!/usr/bin/env bash

# Create folders

mkdir -p ./lib ./snippets/irp-mode

# Tangle Emacs org-mode files

emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "./content/index.org")'
