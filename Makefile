repl:
	pack --with-ipkg dummy-server.ipkg --cg node repl Main.idr

build:
	pack --cg node build dummy-server.ipkg

run:
	node ./build/exec/dummy-server
