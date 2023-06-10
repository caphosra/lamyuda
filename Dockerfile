FROM ghcr.io/caphosra/haskell:latest

COPY . /home/moby/lamdba

RUN \
    cd /home/moby/lamdba; \
    sudo chmod 1777 /tmp; \
    stack build --copy-bins; \
    sudo mv ~/.local/bin/lamdba /usr/local/bin;

ENTRYPOINT ["lamdba"] 
