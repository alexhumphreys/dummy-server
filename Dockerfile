FROM node:16 as build-node

WORKDIR /opt/dummy-server

COPY ./package.json .
COPY ./package-lock.json .

RUN npm install

FROM ghcr.io/stefan-hoeck/idris2-pack:nightly-220609 as build

WORKDIR /opt/dummy-server

COPY ./pack.toml .
COPY ./dummy-server.ipkg .

RUN pack install-deps ./dummy-server.ipkg

COPY src src

RUN pack --cg node build ./dummy-server.ipkg

FROM node:16

WORKDIR /opt/dummy-server

COPY --from=build-node /opt/dummy-server/node_modules /opt/dummy-server/node_modules
COPY --from=build /opt/dummy-server/package.* /opt/dummy-server
COPY --from=build /opt/dummy-server/build/exec/dummy-server /opt/dummy-server

CMD ["node", "./dummy-server"]
