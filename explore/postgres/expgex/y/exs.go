package main

/*
#cgo CFLAGS: -I..
#cgo LDFLAGS:  -L.
#include <expgex.h>
*/
import "C"
import "fmt"
import "unsafe"
import "net/http"
import "time"

var fh C.F0

//export TestGoFunc0
func TestGoFunc0() int {
	fmt.Println("From Golang: Hi!")
	return int(C.f0_call(fh))
}

//export TestGoFunc1
func TestGoFunc1(f unsafe.Pointer) {
	fh = C.F0(f)
}

//export Serve
func Serve() {
	http.HandleFunc("/hello", hello)
	http.ListenAndServe(":8888", nil)
}

func hello(w http.ResponseWriter, r *http.Request) {
	fmt.Println("1", time.Now().Format("2006-01-02 15:04:05"))
	// time.Sleep(5*time.Second)
	fmt.Println("2", time.Now().Format("2006-01-02 15:04:05"))
	w.Write([]byte(fmt.Sprintf("hello world, my PID=%d", TestGoFunc0())))
}

func main() {
	Serve()
}
