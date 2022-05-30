FROM ubuntu:22.04 AS build

RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git && rm -rf /var/lib/apt/lists/*

ENV PATH="~/.pack/bin:~/.idris2/bin:$PATH"

WORKDIR /opt/idris2-pack

ADD idris2-pack/ ./
RUN true

ENV SCHEME=chezscheme

RUN make micropack SCHEME=chezscheme DB=nightly-220530

# WORKDIR /opt/dummy-server

# RUN pack --cg node build ./dummy-server.ipkg
