// em++ -std=c++11 interact.cpp  -Os -s WASM -o interact.wasm --js-library interact.js
// don't do any io, if you don't have WASI
// #include <iostream>

#include <emscripten/emscripten.h>

#ifdef __cplusplus
extern "C" {
#endif

    extern int func_in_js_a(int x);

    int main()
    {
        //std::cout << "[CPP] Hello From C++!\n";
        //std::cout << "[CPP] Value from JS: " << func_in_js_a(1) << "\n";
        return 0;
    }

    int EMSCRIPTEN_KEEPALIVE func_in_cpp_a(int x, int y){
        //std::cout << "[CPP] func_in_cpp_a is called!\n";
        return x + y + func_in_js_a(x);
    }

#ifdef __cplusplus
}
#endif
