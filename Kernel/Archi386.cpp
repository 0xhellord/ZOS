#include "Archi386.h"
#include "pic.h"
#include "pit.h"

static struct l_gdtr		g_idtr;
static struct IDT_DESC	    g_idt_descs[I86_MAX_INTERRUPTS];

static struct l_gdtr		g_gdtr;
static struct GDT_DESC	    g_gdt_descs[6] = { 0 };

static struct TSS_ENTRY     tss_entry = { 0 };


void SetVec(int vec, void* handler)
{
    g_idt_descs[vec].Init((unsigned int)handler, 8, I86_IDT_DESC_BIT32 | I86_IDT_DESC_PRESENT);
}

static void i86_default_handler() 
{
    disable();

    write_string(0x1e, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

    for (;;);
}
void flush_tss(uint16_t sel) {

    _asm {
        mov ax,  sel
        ltr ax
    }
    return;
}
void    InitGDTIDT()
{
    disable();
    g_idtr.base     = &g_idt_descs;
    g_idtr.limit    = sizeof(g_idt_descs) - 1;

    g_gdtr.base     = &g_gdt_descs;
    g_gdtr.limit    = sizeof(g_gdt_descs) - 1;

    g_gdt_descs[0].InitBaseLimit(0, 0);

    g_gdt_descs[1].InitBaseLimit(0, 0xfffff);
    g_gdt_descs[1].Type_DC = 1;
    g_gdt_descs[1].Type_EC = 0;
    g_gdt_descs[1].Type_WR = 1;
    g_gdt_descs[1].Type_AA = 0;

    g_gdt_descs[1].G_AVL = 0;
    g_gdt_descs[1].G_DB = 1;
    g_gdt_descs[1].G_G = 1;
    g_gdt_descs[1].G_L = 0;

    g_gdt_descs[1].P_DPL = 0;
    g_gdt_descs[1].P_Present = 1;
    g_gdt_descs[1].P_System = 1;
    

    g_gdt_descs[2] = g_gdt_descs[1];
    g_gdt_descs[2].Type_DC = 0;
    
    g_gdt_descs[3] = g_gdt_descs[1];
    g_gdt_descs[3].P_DPL = 3;

    g_gdt_descs[4] = g_gdt_descs[1];
    g_gdt_descs[4].P_DPL = 3;
    g_gdt_descs[4].Type_DC = 0;

    g_gdt_descs[5] = g_gdt_descs[1];

    uint32_t base = (uint32_t)&tss_entry;
    uint32_t limit = sizeof(tss_entry);

    g_gdt_descs[5].InitBaseLimit(base, limit);

    g_gdt_descs[5].Type_AA = 1; // TSS Always 1
    g_gdt_descs[5].Type_WR = 0; // TSS Busy
    g_gdt_descs[5].Type_EC = 0; // TSS Alway 0
    g_gdt_descs[5].Type_DC = 1; // TSS 32 Bit
    g_gdt_descs[5].P_System = 0;
    g_gdt_descs[5].P_DPL = 3;
    g_gdt_descs[5].P_Present = 1;

    g_gdt_descs[5].G_AVL = 0;
    g_gdt_descs[5].G_DB = 0;
    g_gdt_descs[5].G_G = 0; //bytes
    g_gdt_descs[5].G_L = 0;

    tss_entry.ss0 = 0x10;
    tss_entry.esp0 = 0x803ffff0;
    
    for (int i = 0; i < I86_MAX_INTERRUPTS; i++)
    {
        g_idt_descs[i].Init((uint32_t)i86_default_handler, 8, I86_IDT_DESC_BIT32 | I86_IDT_DESC_PRESENT);
    }


    //bochs magic breakpoint
    //_asm    xchg bx,bx

    __asm   lidt[g_idtr];
    __asm   lgdt[g_gdtr];


    i86_pic_initialize(0x20, 0x28);
    i86_pit_initialize();
    i86_pit_start_counter(100, I86_PIT_OCW_COUNTER_0, I86_PIT_OCW_MODE_SQUAREWAVEGEN);

    flush_tss(0x2b);
    enable();
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
