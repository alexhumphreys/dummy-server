FROM ghcr.io/stefan-hoeck/idris2-pack:nightly-220607 as build

WORKDIR /opt/dummy-server

COPY src src
COPY ./dummy-server.ipkg .
COPY ./pack.toml .
COPY ./package.json .
COPY ./package-lock.json .

RUN pack --cg node build ./dummy-server.ipkg

FROM node:16

WORKDIR /opt/dummy-server

COPY --from=build /opt/dummy-server/package.* /opt/dummy-server
COPY --from=build /opt/dummy-server/build/exec/dummy-server /opt/dummy-server

RUN npm install

CMD ["node", "./dummy-server"]
