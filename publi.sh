#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"
exec emacs --batch -Q --load build-site.el
