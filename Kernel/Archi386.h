#pragma once
#include "stdint.h"

#pragma pack(push, 1)
struct gdt_entry_bits
{
    unsigned int limit_low : 16;
    unsigned int base_low : 24;
    //attribute byte split into bitfields
    unsigned int accessed : 1;
    unsigned int read_write : 1; //readable for code, writable for data
    unsigned int conforming_expand_down : 1; //conforming for code, expand down for data
    unsigned int code : 1; //1 for code, 0 for data
    unsigned int always_1 : 1; //should be 1 for everything but TSS and LDT
    unsigned int DPL : 2; //priviledge level
    unsigned int present : 1;
    //and now into granularity
    unsigned int limit_high : 4;
    unsigned int available : 1;
    unsigned int always_0 : 1; //should always be 0
    unsigned int big : 1; //32bit opcodes for code, uint32_t stack for data
    unsigned int gran : 1; //1 to use 4k page addressing, 0 for byte addressing
    unsigned int base_high : 8;
};
struct TSS_ENTRY {
    uint32_t prevTss;
    uint32_t esp0;
    uint32_t ss0;
    uint32_t esp1;
    uint32_t ss1;
    uint32_t esp2;
    uint32_t ss2;
    uint32_t cr3;
    uint32_t eip;
    uint32_t eflags;
    uint32_t eax;
    uint32_t ecx;
    uint32_t edx;
    uint32_t ebx;
    uint32_t esp;
    uint32_t ebp;
    uint32_t esi;
    uint32_t edi;
    uint32_t es;
    uint32_t cs;
    uint32_t ss;
    uint32_t ds;
    uint32_t fs;
    uint32_t gs;
    uint32_t ldt;
    uint16_t trap;
    uint16_t iomap;
};

struct GDT_DESC
{
    uint16_t LimitLow16;
    uint16_t BaseLow16;
    uint8_t  BaseMid8;
    //uint8_t  Type:4;

    uint8_t  Type_AA : 1;        //Accesed
    uint8_t  Type_WR : 1;        //Read(Code) / Write(Data)
    uint8_t  Type_EC : 1;        //ExpandDown(Data) / Conform(Code)
    uint8_t  Type_DC : 1;        //1-Code,0-Data
    //uint8_t  p_dpl_s:4;
    
    uint8_t  P_System : 1;
    uint8_t  P_DPL : 2;
    uint8_t  P_Present : 1;

    uint8_t  LimitHigh4:4;

    //uint8_t  g_db_l_avl:4;
    
    uint8_t  G_AVL : 1;
    uint8_t  G_L : 1;     //long mode
    uint8_t  G_DB : 1;    //32 or 16
    uint8_t  G_G : 1;     //Kb or byte

    uint8_t  BaseHigh8;

    void InitBaseLimit(uint32_t base, uint32_t limit)
    {
        BaseLow16   = base & 0x0000FFFF;
        BaseMid8    = (base & 0x00FF0000)>>16;
        BaseHigh8   = (base & 0xFF000000)>>24;

        LimitLow16 = limit & 0x0000FFFF;
        LimitHigh4  = (limit & 0x000F0000)>>16;
    }

};
//! i86 defines 256 possible interrupt handlers (0-255)
#define I86_MAX_INTERRUPTS		256

//! must be in the format 0D110, where D is descriptor type
#define I86_IDT_DESC_BIT16		0x06	//00000110
#define I86_IDT_DESC_BIT32		0x0E	//00001110
#define I86_IDT_DESC_RING1		0x40	//01000000
#define I86_IDT_DESC_RING2		0x20	//00100000
#define I86_IDT_DESC_RING3		0x60	//01100000
#define I86_IDT_DESC_PRESENT		0x80	//10000000

struct IDT_DESC {

    //! bits 0-16 of interrupt routine (ir) address
    uint16_t		baseLo;

    //! code selector in gdt
    uint16_t		sel;

    //! reserved, shold be 0
    uint8_t		    reserved;

    //! bit flags. Set with flags above
    uint8_t		    flags;

    //! bits 16-32 of ir address
    uint16_t		baseHi;

    void Init(uint32_t base, uint16_t cs, uint8_t flag)
    {
        baseLo = base & 0x0000FFFF;
        baseHi = base >> 16;
        sel = cs;
        flags = flag;
    }
};

struct l_gdtr 
{
    uint16_t		limit;
    void*           base;
};

#pragma pack(pop)

void SetVec(int vec, void* handler);
void            DebugUpdateCur(int x, int y);
void    _cdecl  disable();
void    _cdecl  enable();
void    _cdecl  outportb(uint16_t portid, uint8_t value);
uint8_t _cdecl  inportb(uint16_t portid);
void            write_string(int colour, const char *string);
void            InitGDTIDT();