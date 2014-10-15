SRC 	= $(shell find ./src -name '*.purs')
DEPS 	= $(shell find -L bower_components -name '*.purs' | grep src)

build: lib/purescript-immutable.js

run: lib/purescript-immutable.js
	@node $<

clean:
	@rm -rf lib

lib/purescript-immutable.js: $(SRC)
	@mkdir -p $(@D)
	@psc -o $@ $(DEPS) $(^) --main Immutable.Main
