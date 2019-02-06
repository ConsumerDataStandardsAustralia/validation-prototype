nix-build docker.nix -o new-dist/cdr-sandbox.docker
docker load --input result-2
docker run -it --rm cdr-mock-server:latest
