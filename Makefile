default: all
all: test doc


prepare:
	mkdir -p luadoc
clean:
	rm -rf luadoc
test:
	lua -e 'local lcpp = require("lcpp"); lcpp.test();'
run:
	lua -e 'lcpp = require("lcpp"); print("### entered lcpp interactive mode ###")' -i

# DOC
doc: luadoc
ldoc: luadoc
luadoc: prepare
	ldoc.lua ${PWD}
	cp resources/lua-logo-lcpp.png luadoc
