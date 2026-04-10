FROM mcr.microsoft.com/dotnet/sdk:10.0-noble

# Dockerfile - Build a fixed non-root development shell image for local compiler work.

# Install development tools, benchmarking dependencies, and curl for Codex CLI installation
RUN apt-get update && apt-get install -y \
    git \
    vim \
    file \
    less \
    curl \
    sudo \
    nodejs \
    npm \
    htop \
    jq \
    shellcheck \
    bash-completion \
    # Benchmarking tools
    python3 \
    hyperfine \
    valgrind \
    gcc \
    # OCaml tools for benchmarking
    ocaml \
    opam \
    # Emulation for running compiled binaries on non-native hosts
    qemu-user-static \
    && rm -rf /var/lib/apt/lists/*

# Install Codex CLI + Claude Code
RUN npm install -g @openai/codex @anthropic-ai/claude-code

# Repurpose the base image's ubuntu user (UID/GID 1000) as 'dark'.
# UID 1000 matches the default host user on Linux; macOS bind-mounts are permissive.
RUN mkdir -p /workspace && \
    usermod -l dark -d /home/dark -m ubuntu && \
    groupmod -n dark ubuntu && \
    echo "dark ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    mkdir -p /home/dark/.nuget/packages /home/dark/.codex /home/dark/.claude && \
    chown -R dark:dark /home/dark /workspace

# Switch to dark user
USER dark

# Use the image-provided .NET installation as the system runtime/SDK
ENV HOME="/home/dark"
ENV DOTNET_ROOT="/usr/share/dotnet"
ENV DOTNET_CLI_HOME="/home/dark"
ENV DOTNET_MULTILEVEL_LOOKUP="0"

# Add .NET, Rust, dotnet tools and local bin to PATH
ENV PATH="${DOTNET_ROOT}:/home/dark/.dotnet/tools:/home/dark/.local/bin:/home/dark/.cargo/bin:${PATH}"

# Pre-download workload advertising manifests so first-run commands don't fail workload verification.
# Needs elevated privileges because the SDK is installed system-wide under /usr/share/dotnet.
RUN sudo dotnet workload update --advertising-manifests-only --ignore-failed-sources

# Coverage tooling for ./run-coverage
RUN dotnet tool install -g coverlet.console && \
    dotnet tool install -g dotnet-reportgenerator-globaltool

# Install Rust for benchmarking
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Initialize opam and install ocamlfind for benchmarking
RUN opam init --disable-sandboxing --auto-setup --yes && \
    eval $(opam env) && \
    opam install ocamlfind --yes && \
    echo 'eval $(opam env)' >> ~/.bashrc

# Install darklang interpreter from latest GitHub release
COPY --chown=dark:dark scripts/install-darklang-interpreter.sh /tmp/install-darklang-interpreter.sh
RUN bash /tmp/install-darklang-interpreter.sh && \
    rm /tmp/install-darklang-interpreter.sh

# Configure git aliases
RUN git config --global alias.ci commit && \
    git config --global alias.co checkout && \
    git config --global alias.st status

# Configure nice bash prompt with git branch and short path
RUN echo 'parse_git_branch() { git branch 2>/dev/null | grep "^*" | sed "s/* //"; }' >> ~/.bashrc && \
    echo 'short_path() { pwd | sed "s|$HOME|~|"; }' >> ~/.bashrc && \
    echo 'PS1="\[\033[1;32m\]\u@dark\[\033[0m\]:\[\033[1;34m\]\$(short_path)\[\033[0m\]\[\033[1;33m\]\$(parse_git_branch | sed \"s/.*/ (&)/\")\[\033[0m\]\$ "' >> ~/.bashrc

# Enable bash completion for git and other installed tools
RUN echo 'if [ -f /etc/bash_completion ]; then . /etc/bash_completion; fi' >> ~/.bashrc

# Set working directory to the bind-mounted checkout.
WORKDIR /workspace

# Default command: bash shell
CMD ["bash"]
