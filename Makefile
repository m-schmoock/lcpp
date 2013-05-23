default: all

all: test

test:
	lua -e 'local lcpp = require("lcpp"); lcpp.test("Test sucessful");'
