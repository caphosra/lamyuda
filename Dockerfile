FROM ghcr.io/caphosra/haskell:latest

RUN \
    mkdir /home/moby/lamyuda;

COPY . /home/moby/lamyuda

RUN \
    cd /home/moby/lamyuda; \
    sudo chmod 1777 /tmp; \
    stack build --copy-bins; \
    sudo mv ~/.local/bin/lamyuda /usr/local/bin;

ENTRYPOINT ["lamyuda"]
