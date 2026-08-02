#!/usr/bin/env bash
# Runs on every container start: fix volume ownership + docker.sock access.
set -euo pipefail

log() { printf '==> [%s] %s\n' "$(date -u +%H:%M:%SZ)" "$*"; }

# Named volumes are created root-owned; sbt/coursier need write access.
# sbt 2 uses ~/.config/sbt as its global base and ~/.cache/sbt/v2 as the
# machine-wide task cache — neither lives under ~/.sbt anymore.
log "Fixing ownership of cache volumes (slow on a warm Coursier cache)"
for dir in \
  /home/vscode/.config/sbt \
  /home/vscode/.ivy2 \
  /home/vscode/.cache \
  /home/vscode/.cache/coursier \
  /home/vscode/.cache/sbt \
  /home/vscode/.local/share/coursier
do
  sudo mkdir -p "$dir"
done
sudo chown -R vscode:vscode \
  /home/vscode/.config/sbt \
  /home/vscode/.ivy2 \
  /home/vscode/.cache \
  /home/vscode/.local/share/coursier

# Mirror the injected key to disk so clients that attach without Coder's
# environment (plain devcontainer CLI, JetBrains Gateway) still get cache hits.
CREDENTIAL_FILE=/home/vscode/.config/sbt/buildbuddy_credential.txt
if [[ -n "${BUILDBUDDY_API_KEY:-}" ]]; then
  umask 077
  printf 'x-buildbuddy-api-key=%s\n' "$BUILDBUDDY_API_KEY" > "$CREDENTIAL_FILE"
  chmod 600 "$CREDENTIAL_FILE"
fi

log "Wiring up docker.sock"
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

# Bounded: an unresponsive daemon behind the socket blocks `docker info`.
if ! timeout 30s docker info >/dev/null 2>&1; then
  sudo chmod 666 "$SOCK" || true
fi

log "Container start hooks done"
