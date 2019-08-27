PROJECT := postgresql-dynamic

build: default.nix
	nix-build shell.nix

default.nix: package.yaml
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'

shell: default.nix ${PROJECT}.cabal
	nix-shell --command "export PS1='[${PROJECT}] $(value PS1)'; return"

.PHONY: test
test: default.nix ${PROJECT}.cabal
	nix-shell --run 'cabal v1-test'

repl: default.nix
	nix-shell --run 'cabal v1-repl ${PROJECT}'

${PROJECT}.cabal: package.yaml
ifeq ("${IN_NIX_SHELL}",)
	hpack
else
	nix-shell -p haskellPackages.hpack --run 'hpack'
endif
