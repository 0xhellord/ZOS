#include "Archi386.h"

int aa = 0;

int main()
{
    disable();
    InitGDTIDT();
    aa = 1;
    write_string(0x1e, "123456789456123456");
    DebugUpdateCur(15, 0);
    while (aa)
    {
        //__asm   hlt
    }
    return  0;
}
