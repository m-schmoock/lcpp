<p align="center"><a href="http://lua.org"><img src="http://lcpp.schmoock.net/lua-logo-lcpp.png"></a></p>
===

## lcpp - a C PreProcessor in Lua 5.1 for LuaJIT ffi

This module offers a standard preprocessor for C code in pure Lua. 
The primary usecase is to enable LuaJIT ffi preprocessing.
But you can also preprocess any other stuff (even Lua code itself)
    
### Links
 * Project page   http://lcpp.schmoock.net
 * GitHub Page    https://github.com/willsteel/lcpp
 * Lua            http://www.lua.org/
 * LuaJIT         http://luajit.org
 * Sponsored by   http://mmbbq.org

### Usage
    -- load lcpp (ffi.cdef wrapper turned on per default)
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
    
### Supports

    features:
        transparent LuaJIT ffi extension
        replacement macros
        macro chaining
        functional macros
        multiline macros
        expressions (defined(xyz) && ...)
        concat operator ("##")
        screen C single- and multiline comments
        
    directives:
        #include
        #define
        #undef
        #pragma
        #error
        #if 
        #ifdef 
        #ifndef
        #else 
        #else if 
        #elif 
        #endif
        
    macros:
        __LINE__
        __FILE__
        __DATE__
        __TIME__
        __LCPP_INDENT__

### Make targets
    make test      # run the included test cases
    make doc       # ldoc must be installed

### License
MIT licencse included in lua modue.

2012-2013 Michael Schmoock <michael@willigens.de>
