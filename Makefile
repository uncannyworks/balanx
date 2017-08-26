clean:
	rm -rf dist

build-client:
	cd client && nix-shell --run 'cabal configure --ghcjs; cabal build'

build-server:
	cd server && nix-shell --run 'cabal configure; cabal build'

build-resources:
	cp -r client/dist/build/balanx-client/balanx-client.jsexe dist
	cp -rf static/* dist/

build: clean build-client build-server build-resources

gen:
	cd client && cabal2nix . > packages.nix
	cd server && cabal2nix . > packages.nix

run:
	./server/dist/build/balanx-server/balanx-server

setup-db:
	sudo -u root psql -d postgres < sql/db_schema.sql
