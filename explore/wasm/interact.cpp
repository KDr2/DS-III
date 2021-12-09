// standalone:
// em++ -std=c++11 interact.cpp  -Os -s WASM -o thin.wasm --js-library interact-lib.js

// with html+js
// em++ -std=c++11 interact.cpp  -O0 -s WASM -o fat.waso.html --js-library interact-lib.js \
// -DFAT -s EXPORTED_FUNCTIONS="['_free', '_main', '_malloc']"
// don't do any io, if you don't have WASI

#include <iostream>

#include <emscripten/emscripten.h>

#ifdef __cplusplus
extern "C" {
#endif

    extern int func_in_js_a(int x);
    extern int func_in_js_b(void* dest, int x);
    extern int func_in_js_c(void* p);

    int main()
    {
#ifdef FAT
        std::cout << "[CPP] Hello From C++!\n";
        std::cout << "[CPP] Value from JS: " << func_in_js_a(1) << "\n";

        char *data = (char*) malloc(8);
        func_in_js_b(data, 8);
        std::cout << "[CPP] Memory from JS: " << data[1] << data[2] << "\n";

        size_t z = 0;
        func_in_js_c(&z);
        std::cout << "[CPP] size_t from JS: " << z << "\n";
#endif
        return 0;
    }

    int EMSCRIPTEN_KEEPALIVE func_in_cpp_a(int x, int y){
#ifdef FAT
        std::cout << "[CPP] func_in_cpp_a is called!\n";
#endif
        return x + y + func_in_js_a(x);
    }

#ifdef __cplusplus
}
#endif
