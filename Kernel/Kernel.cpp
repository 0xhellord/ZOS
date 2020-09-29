// Kernel.cpp : 此文件包含 "main" 函数。程序执行将在此处开始并结束。
//

void write_string(int colour, const char *string);

void DebugUpdateCur(int x, int y);

int aa = 0;

int main()
{
    aa = 1;
    write_string(0xa, "test print in c");
    DebugUpdateCur(15, 0);
    while (true)
    {

    }
    return  0;
}

void write_string(int colour, const char *string)
{
    volatile char *video = (volatile char*)0xB8000;
    while (*string != 0)
    {
        *video++ = *string++;
        *video++ = colour;
    }
}

//! read byte from device using port mapped io
unsigned char _cdecl inportb(unsigned short portid) {
#ifdef _MSC_VER
    _asm {
        mov		dx, word ptr[portid]
        in		al, dx
        mov		byte ptr[portid], al
    }
#endif
    return (unsigned char)portid;
}


//! write byte to device through port mapped io
void _cdecl outportb(unsigned short portid, unsigned char value) {
#ifdef _MSC_VER
    _asm {
        mov		al, byte ptr[value]
        mov		dx, word ptr[portid]
        out		dx, al
    }
#endif
}


//! enable all hardware interrupts
void _cdecl enable() {
#ifdef _MSC_VER
    _asm sti
#endif
}


//! disable all hardware interrupts
void _cdecl disable() {
#ifdef _MSC_VER
    _asm cli
#endif
}


void DebugUpdateCur(int x, int y) {

    // get location
    unsigned short cursorLocation = y * 80 + x;


    // send location to vga controller to set cursor

    outportb(0x3D4, 14);
    outportb(0x3D5, cursorLocation >> 8); // Send the high byte.
    outportb(0x3D4, 15);
    outportb(0x3D5, (unsigned char)cursorLocation);      // Send the low byte.
}
