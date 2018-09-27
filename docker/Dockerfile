FROM fedora:28
MAINTAINER ricky@elrod.me

RUN dnf clean all && \
    dnf update -y && \
    dnf -y install cabal-install ghc pcre-devel zlib-devel gcc gcc-c++ ruby \
      nodejs coq

RUN useradd -ms /bin/bash eval

USER eval

WORKDIR /home/eval
