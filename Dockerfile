FROM ubuntu:22.04 AS build

RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git gnupg && rm -rf /var/lib/apt/lists/*

ENV PATH "/root/.pack/bin:/root/.idris2/bin:$PATH"

WORKDIR /opt/idris2-pack

ADD idris2-pack/ ./
RUN true

ENV SCHEME=chezscheme

ARG db
RUN make micropack SCHEME=chezscheme DB=$db

WORKDIR /opt/dummy-server

ADD src src
COPY ./dummy-server.ipkg .
COPY ./package.json .
COPY ./package-lock.json .

RUN pack --cg node build ./dummy-server.ipkg

FROM node:16

WORKDIR /opt/dummy-server

COPY --from=build /opt/dummy-server/package.* /opt/dummy-server
COPY --from=build /opt/dummy-server/build/exec/dummy-server /opt/dummy-server

RUN npm install
