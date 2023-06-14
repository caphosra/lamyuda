FROM ghcr.io/caphosra/haskell:latest AS build

RUN \
    mkdir /home/moby/lamyuda;

COPY . /home/moby/lamyuda

RUN \
    cd /home/moby/lamyuda; \
    sudo chmod 1777 /tmp; \
    stack build --copy-bins;

FROM ubuntu:20.04 AS ship

RUN \
    apt update; \
    apt install -y build-essential; \
    apt clean; \
    rm -rf /var/lib/apt/lists/*;

COPY --from=build /home/moby/.local/bin/lamyuda /usr/local/bin

ENTRYPOINT ["lamyuda"]
