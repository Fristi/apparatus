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

USER vscode
ENV COURSIER_BIN_DIR=/home/vscode/.local/share/coursier/bin
ENV PATH="${COURSIER_BIN_DIR}:${PATH}"
RUN cs install sbt metals scala-cli \
  && printf '\nexport PATH="$HOME/.local/share/coursier/bin:$PATH"\n' >> /home/vscode/.bashrc \
  && printf '\nexport PATH="$HOME/.local/share/coursier/bin:$PATH"\n' >> /home/vscode/.zshrc

USER root
