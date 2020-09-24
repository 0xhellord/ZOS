
EnableA20:

    cli

    call    a20wait
    mov     al,0xAD
    out     0x64,al

    call    a20wait
    mov     al,0xD0
    out     0x64,al

    call    a20wait2
    in      al,0x60
    push    eax

    call    a20wait
    mov     al,0xD1
    out     0x64,al

    call    a20wait
    pop     eax
    or      al,2
    out     0x60,al

    call    a20wait
    mov     al,0xAE
    out     0x64,al

    call    a20wait
    sti
    ret

a20wait:
    in      al,0x64
    test    al,2
    jnz     a20wait
    ret


a20wait2:
    in      al,0x64
    test    al,1
    jz      a20wait2
    ret


gdt_start:
    dd 0x0
    dd 0x0

gdt_code: 
    dw 0xFFFF
    dw 0x0
    db 0x0
    db 10011010b
    db 11001111b
    db 0x0       

gdt_data:
    dw 0xFFFF
    dw 0x0
    db 0x0
    db 10010010b
    db 11001111b
    db 0x0

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1 
    dd gdt_start

CODE_SEG equ gdt_code - gdt_start
DATA_SEG equ gdt_data - gdt_start


InitGDT:
	cli                  
	pusha
	lgdt 	[gdt_descriptor]
	sti
	popa
	ret





;[Table 4-3.  Use of CR3 with 32-Bit Paging]
;-Bit     Contents-
; 2:0     Ignored
; 3 (PWT) Page-level write-through;  (see Section 4.9)
; 4 (PCD) Page-level cache disable;  (see Section 4.9)
; 11:5    Ignored
; 31:12   Physical address of the 4-KByte aligned page directory used for linear-address translation
; 63:32   Ignored (these bits exist only on processors supporting the Intel-64 architecture)

;[Table 4-5.  Format of a 32-Bit Page-Directory Entry that References a Page Table]
;-Bit         Contents-
; 0 (P)       Present; must be 1 to reference a page table
; 1 (R/W)     Read/write; if 0, writes may not be allowed to the 4-MByte region controlled by this entry (see Section 4.6)
; 2 (U/S)     User/supervisor; if 0, user-mode accesses are not allowed to the 4-MByte region controlled by this entry (see Section 4.6)
; 3 (PWT)     Page-level write-through; (see Section 4.9)
; 4 (PCD)     Page-level cache disable; (see Section 4.9)
; 5 (A)       Accessed; indicates whether this entry has been used for linear-address translation (see Section 4.8)
; 6           Ignored
; 7 (PS)      If CR4.PSE = 1, must be 0 (otherwise, this entry maps a 4-MByte page; see Table 4-4); otherwise, ignored
; 11:8        Ignored
; 31:12       Physical address of 4-KByte aligned page table referenced by this entry

; [Table 4-6.  Format of a 32-Bit Page-Table Entry that Maps a 4-KByte Page]
; -Bit          Contents-
; 0 (P)         Present; must be 1 to map a 4-KByte page
; 1 (R/W)       Read/write; if 0, writes may not be allowed to the 4-KByte page referenced by this entry (see Section 4.6)
; 2 (U/S)       User/supervisor; if 0, user-mode accesses are not allowed to the 4-KByte page referenced by this entry (see Section 4.6)
; 3 (PWT)       Page-level write-through;  (see Section 4.9)
; 4 (PCD)       Page-level cache disable;  (see Section 4.9)
; 5 (A)         Accessed; indicates whether software has accessed the 4-KByte page referenced by this entry (see Section 4.8)
; 6 (D)         Dirty; indicates whether software has written to the 4-KByte page referenced by this entry (see Section 4.8)
; 7 (PAT)       If the PAT is supported, indirectly determines the memory type used to access the 4-KByte page referenced by this entry (see Section 4.9.2); otherwise, reserved (must be 0)1
; 8 (G)         Global; if CR4.PGE = 1, determines whether the translation is global (see Section 4.10); ignored otherwise
; 11:9          Ignored
; 31:12         Physical address of the 4-KByte page referenced by this entry