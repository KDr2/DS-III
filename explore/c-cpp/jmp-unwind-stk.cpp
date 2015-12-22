#include <iostream>
#include <setjmp.h>

class Clazz{
public:
    Clazz(){
        std::cout << "CTOR" << std::endl;
    }
    virtual ~Clazz(){
        std::cout << "DTOR" << std::endl;
    }
};
jmp_buf env;

void Step0(){
    Clazz z;// not unwind
    longjmp(env,1);
}

int main(int argc, char *argv[]){
    //Clazz z;
    volatile int i = 1;
    i += setjmp(env);
    if(i<3)
        Step0();
    return 0;
}
