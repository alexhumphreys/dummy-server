FROM ubuntu:22.04 AS build

RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git && rm -rf /var/lib/apt/lists/*

ENV PATH "/root/.pack/bin:/root/.idris2/bin:$PATH"

WORKDIR /opt/idris2-pack

ADD idris2-pack/ ./
RUN true

ENV SCHEME=chezscheme

RUN make micropack SCHEME=chezscheme DB=nightly-220530

WORKDIR /opt/dummy-server

COPY . .

RUN pack --cg node build ./dummy-server.ipkg

RUN apt-get update && apt-get install --yes gnupg
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 68576280
RUN echo "deb https://deb.nodesource.com/node_16.x jammy main" >> /etc/apt/sources.list.d/node.list
RUN apt-get update
RUN apt-get install nodejs --yes
