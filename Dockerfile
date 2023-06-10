FROM ghcr.io/caphosra/haskell:latest

RUN \
    cd ~; \
    mkdir lamdba;

COPY . ~/lamdba

RUN \
    cd ~/lamdba; \
    sudo chmod 1777 /tmp; \
    stack build --copy-bins; \
    sudo mv ~/.local/bin/lamdba /usr/local/bin;

ENTRYPOINT ["lamdba"] 
