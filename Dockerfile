FROM mcr.microsoft.com/dotnet/sdk:9.0-noble

# Install development tools and curl for Claude Code installation
RUN apt-get update && apt-get install -y \
    git \
    vim \
    file \
    less \
    curl \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Create project directory structure as root
RUN mkdir -p /Users/paulbiggar/projects

# Create paulbiggar user with same UID as host (501) for file permissions
RUN useradd -m -u 501 -s /bin/bash paulbiggar && \
    echo "paulbiggar ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    chown -R paulbiggar:paulbiggar /Users/paulbiggar

# Switch to paulbiggar user
USER paulbiggar

# Install Claude Code CLI as paulbiggar user
RUN curl -fsSL https://claude.ai/install.sh -o /tmp/install.sh && \
    bash /tmp/install.sh && \
    rm /tmp/install.sh

# Add Claude Code to PATH
ENV PATH="/home/paulbiggar/.local/bin:${PATH}"

# Set working directory to match host path
WORKDIR /Users/paulbiggar/projects/compiler-for-dark

# Container will use volume mount for source code
# No COPY needed - source comes from host via volume

# Default command: bash shell
CMD ["bash"]
