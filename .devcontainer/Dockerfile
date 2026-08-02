FROM mcr.microsoft.com/devcontainers/java:1-21-bookworm

ARG TARGETARCH
ARG DOCKER_CLI_VERSION=29.7.0

# Arch-independent launcher (self-executing jar). Native cs-*-pc-linux.gz
# assets are x86_64-only in current coursier releases, which breaks arm64.
RUN curl -fL -o /usr/local/bin/cs \
      https://github.com/coursier/coursier/releases/latest/download/coursier \
  && chmod +x /usr/local/bin/cs

# Static Docker CLI only — daemon is the host's via mounted docker.sock.
# Avoids apt docker.io / DinD features that fail under JetBrains builds.
RUN DOCKER_ARCH="$( [ "${TARGETARCH}" = "arm64" ] && echo aarch64 || echo x86_64 )" \
  && curl -fL "https://download.docker.com/linux/static/stable/${DOCKER_ARCH}/docker-${DOCKER_CLI_VERSION}.tgz" \
    | tar -xz -C /tmp \
  && install -m 0755 /tmp/docker/docker /usr/local/bin/docker \
  && rm -rf /tmp/docker \
  && groupadd -f docker \
  && usermod -aG docker vscode

# Login shells reset PATH from /etc/profile, so the image-wide ENV below is not
# enough for terminals — profile.d restores it for bash and zsh alike.
RUN echo 'export PATH="$HOME/.local/share/coursier/bin:$PATH"' \
      > /etc/profile.d/coursier.sh

# Envbuilder on Coder never applies customizations.vscode, and the VS Code server
# it talks to is installed on ephemeral storage after post-start.sh has run. Bake
# the extensions into the image instead; post-start.sh copies them into the
# server's extensions directory, which the server rescans on startup.
ARG METALS_VERSION=1.69.0
ARG SCALA_EXTENSION_VERSION=0.5.10
RUN set -eu; \
    mkdir -p /opt/vscode-extensions; \
    seed_extension() { \
      pub="$1"; name="$2"; ver="$3"; \
      curl -fsSL -o /tmp/ext.vsix \
        "https://open-vsx.org/api/$pub/$name/$ver/file/$pub.$name-$ver.vsix"; \
      unzip -q /tmp/ext.vsix -d /tmp/ext; \
      mv /tmp/ext/extension "/opt/vscode-extensions/$pub.$name-$ver"; \
      rm -rf /tmp/ext /tmp/ext.vsix; \
    }; \
    seed_extension scalameta metals "${METALS_VERSION}"; \
    seed_extension scala-lang scala "${SCALA_EXTENSION_VERSION}"

USER vscode
ENV COURSIER_BIN_DIR=/home/vscode/.local/share/coursier/bin
ENV PATH="${COURSIER_BIN_DIR}:${PATH}"
RUN cs install sbt metals scala-cli \
  && cs install --contrib cellar

USER root
