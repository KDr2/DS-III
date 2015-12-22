package go1

/*
   #include <stdio.h>
   #include <stdlib.h>
   void hello() {
        printf("Hello, World, from C!\n");
   }
*/
import "C"

import "fmt"
import "kdr2.com/newmath"

type V interface {}
type S1 struct {
	i int
	f int
}

var GENDER [3]string = [3]string{"male", "female", "other"}

func main() {
	fmt.Printf("Hello, world.\n")
	fmt.Printf("Hello, world.  Sqrt(2) = %v\n", newmath.Sqrt(2))
	C.hello()
	var x1 V = 1;
	x2 := int(x1.(int));
	fmt.Printf("%v\n", x2);
	
	s1 := S1{}
	s2 := s1
	fmt.Printf("%p, %p, %p\n", &s1, &s1, &s2);
}
