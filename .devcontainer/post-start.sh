#!/usr/bin/env bash
# Runs on every container start: fix volume ownership + docker.sock access.
set -euo pipefail

# Named volumes are created root-owned; sbt/coursier need write access.
for dir in \
  /home/vscode/.sbt \
  /home/vscode/.ivy2 \
  /home/vscode/.cache \
  /home/vscode/.cache/coursier \
  /home/vscode/.local/share/coursier
do
  sudo mkdir -p "$dir"
done
sudo chown -R vscode:vscode \
  /home/vscode/.sbt \
  /home/vscode/.ivy2 \
  /home/vscode/.cache \
  /home/vscode/.local/share/coursier

SOCK=/var/run/docker.sock
if [[ ! -S "$SOCK" ]]; then
  echo "warning: $SOCK not mounted — Testcontainers/docker will not work"
  exit 0
fi

SOCK_GID="$(stat -c '%g' "$SOCK" 2>/dev/null || stat -f '%g' "$SOCK")"
if getent group "$SOCK_GID" >/dev/null; then
  GROUP_NAME="$(getent group "$SOCK_GID" | cut -d: -f1)"
else
  GROUP_NAME=dockerhost
  sudo groupadd -g "$SOCK_GID" "$GROUP_NAME" 2>/dev/null \
    || GROUP_NAME="$(getent group "$SOCK_GID" | cut -d: -f1)"
fi
sudo usermod -aG "$GROUP_NAME" vscode 2>/dev/null || true

if ! docker info >/dev/null 2>&1; then
  sudo chmod 666 "$SOCK" || true
fi
