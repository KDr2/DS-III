
// actually we don't need this file when we import and
// run a WASM lib, if we define the real function in other
// file


// this definition is not necessarily palced here
function func_in_js_a(i) {
    console.log("[JS] func_in_js_a is called");
    return 2 * i;
}

var dummy_func = function () {};

if (typeof mergeInto !== 'undefined') {
    mergeInto(LibraryManager.library, {
        // we just a dummy function, and we will
        // define the real function somewhere else.
        func_in_js_a: dummy_func,
    });
}
