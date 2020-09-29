#include "Archi386.h"

int aa = 0;

int main()
{
    disable();
    InitGDTIDT();
    aa = 1;
    write_string(0x1e, "oo");
    DebugUpdateCur(15, 0);
    while (aa)
    {

    }
    return  0;
}
