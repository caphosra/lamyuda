FROM ghcr.io/caphosra/haskell:latest

WORKDIR ~/lamdba;

COPY . ~/lamdba

RUN sudo chmod 1777 /tmp
RUN stack build --copy-bins
RUN sudo mv ~/.local/bin/lamdba /usr/local/bin

ENTRYPOINT ["lamdba"] 
