#include "Archi386.h"

static struct IDT_DESC	    g_idt_descs[256] = {0};
static struct l_gdtr		g_idtr = { 0 };

static struct GDT_DESC	    g_gdt_descs[3] = { 0 };
static struct l_gdtr		g_gdtr = { 0 };

void    InitGDTIDT()
{
 
    g_gdtr.base     = &g_gdt_descs;
    g_gdtr.limit    = sizeof(g_gdt_descs) - 1;

    g_gdt_descs[0].Init(0, 0, 0, 0, 0);
    g_gdt_descs[1].Init(0, 0xfffff, 10, 12, 9);
    g_gdt_descs[2].Init(0, 0xfffff, 2, 12, 9);

    //bochs magic breakpoint
    //_asm    xchg bx,bx

    __asm   lgdt [g_gdtr]
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
uint8_t _cdecl inportb(uint16_t portid) 
{
#ifdef _MSC_VER
    _asm {
        mov		dx, word ptr[portid]
        in		al, dx
        mov		byte ptr[portid], al
    }
#endif
    return (uint8_t)portid;
}


//! write byte to device through port mapped io
void _cdecl outportb(uint16_t portid, uint8_t value)
{
#ifdef _MSC_VER
    _asm {
        mov		al, byte ptr[value]
        mov		dx, word ptr[portid]
        out		dx, al
    }
#endif
}


//! enable all hardware interrupts
void _cdecl enable() 
{
#ifdef _MSC_VER
    _asm sti
#endif
}


//! disable all hardware interrupts
void _cdecl disable() 
{
#ifdef _MSC_VER
    _asm cli
#endif
}


void DebugUpdateCur(int x, int y) 
{

    // get location
    unsigned short cursorLocation = y * 80 + x;


    // send location to vga controller to set cursor

    outportb(0x3D4, 14);
    outportb(0x3D5, cursorLocation >> 8); // Send the high byte.
    outportb(0x3D4, 15);
    outportb(0x3D5, (unsigned char)cursorLocation);      // Send the low byte.
}
