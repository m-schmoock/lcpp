package = "lcpp.git"
version = "git HEAD"
rockspec_format = "1.0"

source = {
   url = "git://github.com/willsteel/lcpp.git",
}

description = {
   summary = "a C PreProcessor in Lua 5.1 for LuaJIT ffi",
   detailed = [[
      This module offers a standard preprocessor for C code in pure Lua. 
      The primary usecase is to enable LuaJIT ffi preprocessing. 
      But you can also preprocess any other stuff (even Lua code itself).
      github site: (https://github.com/willsteel/lcpp)
   ]],
   homepage = "http://lcpp.schmoock.net/",
   license = "MIT",
}

dependencies = {
   "lua >= 5.1",
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
