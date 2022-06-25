PACK_DB="nightly-220607"

repl:
	rlwrap pack --with-ipkg dummy-server.ipkg --cg node repl src/Main.idr

install-node-deps:
	npm install

.PHONY : build
build:
	pack --cg node build dummy-server.ipkg

docker-build:
	docker build --build-arg db=$(PACK_DB) -t snazzybucket/dummy-server .

docker-run:
	docker run --rm -it snazzybucket/dummy-server /bin/bash

docker-compose-restart:
	docker compose down
	docker compose build
	docker compose up

clean:
	rm -rf ./build
	rm -rf ./node_modules

run-db:
	docker run -p 5432:5432 --name some-postgres -e POSTGRES_PASSWORD=mysecretpassword -d postgres

kill-db:
	docker rm -f some-postgres

run:
	PGUSER=postgres \
	PGHOST=127.0.0.1 \
	PGPASSWORD=mysecretpassword \
	PGDATABASE=foo \
	PGPORT=5432 \
	node ./build/exec/dummy-server

frontend-repl:
	rlwrap -n pack --with-ipkg ./src/Frontend/frontend.ipkg --cg javascript repl ./src/Frontend/Main.idr

frontend:
	pack build ./src/Frontend/frontend.ipkg
	mkdir -p static/js
	cp ./src/Frontend/build/exec/frontend.js static/js/frontend.js
