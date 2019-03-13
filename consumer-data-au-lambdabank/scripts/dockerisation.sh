nix-build docker.nix -o dist-newstyle/mock-dh.docker
docker load --input dist-newstyle/mock-dh.docker
docker run -it --rm mock-dh:latest
