lcpp
====

A Lua C PreProcessor

This module offers a standard preprocessor for C code in pure Lua. 
The primary usecase is to enable LuaJIT ffi preprocessing.
But you can also preprocess any other stuff (even Lua code itself)


## Usage
    local lcpp = require("lcpp")
    local result = lcpp.compile("...")

## Make targets
    make test      # run the included test cases
