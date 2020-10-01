#include "Archi386.h"

int aa = 0;
// define our structure

void __cdecl memset(void* _Dst, int _Val, uint32_t _Size)
{
    char* p = (char*)_Dst;
    for (uint32_t i =0; i < _Size; i++)
    {
        *(p + i) = _Val;
    }
}
// tell compiler our int32 function is external
typedef void ( _cdecl *int32)(unsigned char intnum, regs16_t* regs);

// int32 test
void int32_test(int32 pInt32)
{
    int y;
    regs16_t regs;

    // switch to 320x200x256 graphics mode
    regs.ax = 0x0013;
    pInt32(0x10, &regs);

    // full screen with blue color (1)
    memset((char*)0xA0000, 1, (320 * 200));

    // draw horizontal line from 100,80 to 100,240 in multiple colors
    for (y = 0; y < 200; y++)
        memset((char*)0xA0000 + (y * 320 + 80), y, 160);

    // wait for key
    regs.ax = 0x0000;
    pInt32(0x16, &regs);

    // switch to 80x25x16 text mode
    regs.ax = 0x0003;
    pInt32(0x10, &regs);
}
int main(void* pInt32)
{
    disable();
    InitGDTIDT();


 
    // _asm xchg bx, bx
    int32_test((int32)pInt32);

    

    aa = 1;
    write_string(0x1e, "123456789456123456");
    DebugUpdateCur(15, 0);
    //_asm xchg bx, bx
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
