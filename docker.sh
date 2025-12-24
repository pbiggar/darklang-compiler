#!/bin/bash
# Convenience script for Docker operations

set -e

case "$1" in
  build)
    echo "Building Docker image..."
    docker compose build
    ;;

  up)
    echo "Starting container..."
    docker compose up -d
    echo "Container started. Use './docker.sh shell' to enter."
    ;;

  down)
    echo "Stopping container..."
    docker compose down
    ;;

  shell)
    echo "Entering container shell..."
    docker compose exec dev bash
    ;;

  restart)
    echo "Restarting container..."
    docker compose down
    docker compose up -d
    echo "Container restarted."
    ;;

  logs)
    docker compose logs -f dev
    ;;

  clean)
    echo "Cleaning build artifacts in container..."
    docker compose exec dev dotnet clean
    ;;

  build-compiler)
    echo "Building compiler in container..."
    docker compose exec dev dotnet build
    ;;

  status)
    echo "Container status:"
    docker compose ps
    ;;

  *)
    echo "Docker Development Environment"
    echo ""
    echo "Usage: ./docker.sh {command}"
    echo ""
    echo "Commands:"
    echo "  build           Build Docker image"
    echo "  up              Start container (detached)"
    echo "  down            Stop container"
    echo "  shell           Enter container shell"
    echo "  restart         Restart container"
    echo "  logs            View container logs"
    echo "  clean           Clean build artifacts"
    echo "  build-compiler  Build compiler in container"
    echo "  status          Show container status"
    exit 1
    ;;
esac
