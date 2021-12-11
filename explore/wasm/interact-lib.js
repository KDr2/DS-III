
// actually we don't need this file when we import and
// run a WASM lib, if we define the real function in other
// file


// this definition is not necessarily palced here
function func_in_js_a(i) {
    console.log("[JS] func_in_js_a is called");
    return 2 * i;
}

function func_in_js_b(dest, l) {
    console.log("[JS] func_in_js_b is called");
    var d = new Uint8Array(l);
    d[0] = 97;
    d[1] = 98;
    d[2] = 99;
    d[3] = 100;
    HEAP8.set(d, dest);
}

function func_in_js_c(dest) {
    console.log("[JS] func_in_js_c is called");
    var d = new Uint8Array(4);
    var n = 123128;
    d[3] = n >> 24 & 0xff;
    d[2] = n >> 16 & 0xff;
    d[1] = n >> 8  & 0xff;
    d[0] = n >> 0  & 0xff;

    HEAP8.set(d, dest);
}

function func_in_js_d(dest) {
    console.log("[JS] func_in_js_d is called");
    var d = new Uint32Array(1);
    d[0] = 123128;
    HEAP8.set(d, dest);
}

var dummy_func = function () {};

if (typeof mergeInto !== 'undefined') {
    mergeInto(LibraryManager.library, {
        // we just a dummy function, and we will
        // define the real function somewhere else.
        // func_in_js_a: dummy_func,
        func_in_js_a: func_in_js_a,
        func_in_js_b: func_in_js_b,
        func_in_js_c: func_in_js_c,
        func_in_js_d: func_in_js_d,
    });
}
