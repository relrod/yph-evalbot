FROM centos:7
MAINTAINER ricky@elrod.me


RUN curl -o /etc/yum.repos.d/ghc.repo \
      https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.0.2/repo/epel-7/petersen-ghc-8.0.2-epel-7.repo && \
      sed -i s/gpgcheck=1/gpgcheck=0/ /etc/yum.repos.d/ghc.repo

RUN yum clean all && \
    yum update -y && \
    yum -y install epel-release && \
    yum -y install cabal-install ghc pcre-devel zlib-devel gcc gcc-c++ ruby \
      nodejs

RUN curl -o /etc/yum.repos.d/coq.repo \
    https://copr.fedorainfracloud.org/coprs/codeblock/coq-epel/repo/epel-7/codeblock-coq-epel-epel-7.repo && \
    sed -i s/gpgcheck=1/gpgcheck=0/ /etc/yum.repos.d/coq.repo && \
    yum -y install coq

RUN useradd -ms /bin/bash eval

USER eval

WORKDIR /home/eval
