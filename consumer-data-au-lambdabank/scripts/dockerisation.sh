nix-build docker.nix -o dist-newstyle/cdr-sandbox.docker
docker load --input dist-newstyle/cdr-sandbox.docker
docker run -it --rm cdr-mock-server:latest
