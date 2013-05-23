lcpp
====

## a C PreProcessor in Lua 5.1 for LuaJIT ffi

This module offers a standard preprocessor for C code in pure Lua. 
The primary usecase is to enable LuaJIT ffi preprocessing.
But you can also preprocess any other stuff (even Lua code itself)

### Links
 * Project page   http://lcpp.schmoock.net
 * GitHub Page    https://github.com/willsteel/lcpp
 * LuaJIT         http://luajit.org
 * Sponsored by   http://mmbbq.org

### Usage
    -- load lcpp (ffi and tests turned on per default)
    local lcpp = require("lcpp")
        
    -- just use LuaJIT ffi and lcpp together
    ffi.cdef("#include <your_header.h>")

    -- or compile some code by hand
    local result = lcpp.compile("...")
    local out = lcpp.compile([[
        #define MAXPATH 260
        typedef struct somestruct_t {
            void*          base;
            size_t         size;
            wchar_t        path[MAXPATH];
        } t_exe;
    ]])

### Make targets
    make test      # run the included test cases
    make doc       # ldoc must be installed
