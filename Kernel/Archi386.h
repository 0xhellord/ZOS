#pragma once
#include "stdint.h"

#pragma pack(push, 1)


struct GDT_DESC
{
    uint16_t LimitLow;
    uint16_t BaseLow16;
    uint8_t  BaseMid8;
    uint8_t  Type:4;
    uint8_t  p_dpl_s:4;
    uint8_t  LimitHigh4:4;
    uint8_t  g_db_l_avl:4;
    uint8_t  BaseHigh8;

    void Init(uint32_t base, uint32_t limit, uint8_t TYPE_DC_EC_WR_AA, uint8_t G_DB_L_AVL, uint8_t P_DPL_S)
    {
        BaseLow16   = base & 0x0000FFFF;
        BaseMid8    = (base & 0x00FF0000)>>16;
        BaseHigh8   = (base & 0xFF000000)>>24;

        LimitLow    = limit & 0x0000FFFF;
        LimitHigh4  = (limit & 0x000F0000)>>16;

        Type        = TYPE_DC_EC_WR_AA;

        g_db_l_avl  = G_DB_L_AVL;
        p_dpl_s     = P_DPL_S;
    }

};


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
};

struct l_gdtr 
{
    uint16_t		limit;
    void*           base;
};

#pragma pack(pop)


void            DebugUpdateCur(int x, int y);
void    _cdecl  disable();
void    _cdecl  enable();
void    _cdecl  outportb(uint16_t portid, uint8_t value);
uint8_t _cdecl  inportb(uint16_t portid);
void            write_string(int colour, const char *string);
void            InitGDTIDT();