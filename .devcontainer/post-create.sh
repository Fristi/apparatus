#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

# Named volumes start empty (and root-owned until post-start chown).
echo "==> Ensuring Coursier apps (sbt, metals, scala-cli)"
cs install sbt metals scala-cli

echo "==> Installing npm dependencies (VitePress)"
npm ci

echo "==> Warming sbt (downloads launcher + project deps)"
sbt --batch -Dsbt.supershell=false "exit"

echo "==> Dev container ready"
echo "    Scala tests:  sbt tests/test"
echo "    Docs site:    sbt docs/mdoc && npm run docs:dev"
