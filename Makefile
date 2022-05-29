repl:
	rlwrap pack --with-ipkg dummy-server.ipkg --cg node repl Main.idr

install-node-deps:
	npm install

.PHONY : build
build:
	pack --cg node build dummy-server.ipkg

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
	PGDATABASE=postgres \
	PGPORT=5432 \
	node ./build/exec/dummy-server
