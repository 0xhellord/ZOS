#pragma once

#pragma pack(push, 1)
struct GDT_DESC
{
    unsigned    short   limit_low_16;
    unsigned    short   base_low_24;
    unsigned    char    base_low_24_1;

    struct _X
    {
        struct 
        {
            unsigned char   accessed : 1;
            unsigned char   rw : 1;
            union
            {
                unsigned    char    direction : 1;
                unsigned    char    comform : 1;
            }DC;
            unsigned char   executable : 1;
        }type;
        unsigned    char    SystemSegment : 1;
        unsigned    char    DPL : 2;
        unsigned    char    Present : 1;
    }a;

    
    struct MyStruct
    {
        unsigned    char    Limit_high_4 : 4;
        unsigned    char    AVL : 1;
        unsigned    char    LongMode : 1;
        unsigned    char    DB_Default_Bits : 1;
        unsigned    char    Granularity : 1;
    }b;

    unsigned    char    base_high_8 : 8;
};


struct IDT_DESC {

    //! bits 0-16 of interrupt routine (ir) address
    unsigned    short		baseLo;

    //! code selector in gdt
    unsigned    short		sel;

    //! reserved, shold be 0
    unsigned    char		reserved;

    //! bit flags. Set with flags above
    unsigned    char		flags;

    //! bits 16-32 of ir address
    unsigned    short		baseHi;
};

struct l_gdtr 
{
    unsigned    short		limit;
    unsigned    int 		base;
};

#pragma pack(pop)


void DebugUpdateCur(int x, int y);
void _cdecl disable();
void _cdecl enable();
void _cdecl outportb(unsigned short portid, unsigned char value);
unsigned char _cdecl inportb(unsigned short portid);
void write_string(int colour, const char *string);
void    InitGDTIDT();