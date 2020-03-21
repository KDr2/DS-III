
#include "lua.h"
#include "lauxlib.h"

/*
  test yield crosse context:

  lf0 -> call cf1
  cf1 -> call lf1
  lf1 -> yield to lf0
  lf0 -> resume lf1 then go back to cf1
  cf1 -> return to lf0
 */


static int call_lua_func(lua_State *L, const char *funcname)
{

    lua_getglobal(L, funcname);
    if(!lua_isfunction(L, -1)){
        lua_pop(L, 1);
        printf("no func\n");
        return 0;
    }
    lua_pushnumber(L, 1234);

        /* do the call (1 arguments, 1 result) */
    if (lua_pcall(L, 1, 1, 0) != 0) {
        printf("call error\n");
        return 0;
    }
    printf("return form %s :%d\n",funcname, lua_tonumber(L, -1));
    lua_pop(L, 1);
    return 1;
}


static int xcorot_cf1(lua_State *L) {
    printf("enter cf1\n");
    const char *fn=lua_tostring(L, -1);
    printf("\twill call %s\n",fn);
    lua_pop(L, 1);
    int i= call_lua_func(L, fn);
    printf("\taftter call %s=%d\n",fn, i);
    lua_pushboolean(L, 1);
    return 1;
}

static const luaL_Reg xcorot[] = {
    {"cf1", xcorot_cf1},
    {NULL, NULL}
};


LUALIB_API int luaopen_xcorot (lua_State *L) {
    luaL_register(L, "xcorot", xcorot);
    return 1;
}
