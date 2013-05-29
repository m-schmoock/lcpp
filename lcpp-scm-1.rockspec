package = "lcpp"
version = "scm-1"
rockspec_format = "1.0"

source = {
   url = "git://github.com/willsteel/lcpp.git",
}

description = {
   summary = "a C PreProcessor in Lua 5.1 for LuaJIT ffi",
   detailed = [[
      This module offers a standard preprocessor for C code in pure Lua. 
      The primary usecase is to enable LuaJIT ffi preprocessing. 
      But you can also preprocess any other data (even Lua code itself).
      github site: (https://github.com/willsteel/lcpp)
   ]],
   homepage = "http://lcpp.schmoock.net/",
   license = "MIT/X11",
}

dependencies = {
   "lua >= 5.1",  -- luajit >= 2.0.0 is recommended but not required
}

build = {
   type = "none",
   install = {
      lua = {
         lcpp = "lcpp.lua",
      },
   },
   copy_directories = {},
}
