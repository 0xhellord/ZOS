#include "Archi386.h"

int aa = 0;

int main()
{
    //disable();
    InitGDTIDT();
    aa = 1;
    write_string(0x1e, "123456789456123456");
    DebugUpdateCur(15, 0);
    _asm xchg bx, bx
    *(uint8_t*)0x400000 = 0xeb;
    *(uint8_t*)0x400001 = 0xfe;
   
    __asm
    {
        mov ax, 0x23
        mov ds, ax      //user mode ds,es,fs,gs
        mov es, ax
        mov fs, ax
        mov gs, ax      

        push 0x23       //user mode ss 
        push esp        //user mode esp
        pushfd          //eflags
        push 0x1b       //user mode cs
        push 0x400000   //user mode eip
        iretd
    }
user_start:
    //_asm xchg bx,bx

    while (1)
    {
        //__asm   hlt
    }
    return  0;
}
