project := $(shell basename $$(pwd))

build: default.nix
	nix-build shell.nix

default.nix: package.yaml
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'

shell: default.nix
	nix-shell --command "export PS1='[${project}] \t \# \h $$? $$ '; return"

.PHONY: test
test: default.nix
	nix-shell --run 'cabal v1-test'

repl: default.nix
	nix-shell --run 'cabal v1-repl crispy-broccoli'
