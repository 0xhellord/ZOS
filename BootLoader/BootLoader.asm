[bits 16]

org 0x8000

jmp start

LOGO        db "loader!!", 0
IOError     db "Disk IO Failed", 0xd , 0xa , 0

; AL:Sector Count CL: Sector ES:BX: Buffer
ReadSector:
	mov ch, 0           ; Cylinder
	mov dh, 0           ; Head
                        ; DL = drive number passed from BIOS
	mov ah, 2
	int 13h
	jnc OK
	
	push IOError
	call PrintString
OK:
	ret

PrintEndLine:
    push    0xd
    call    PrintChar
    push    0xa
    call    PrintChar
    ret;

PrintChar:
    push    si
    mov     si, sp
    mov     al, [si+4]
    mov     ah, 0xe
    int     0x10
    pop     si
    ret     2

PrintString:
    push    si
    mov     si, sp
    mov     si, [si+4]
    mov     ah, 0xe
    cld

    REPEAT:
        lodsb
        test    al, al
        jz      END
        int     0x10
        jmp     REPEAT

    END:
        call    PrintEndLine
    pop     si
    ret     2;

%include    "protectmode.asm"

start:
    push        LOGO
    call        PrintString
    call        EnableA20
    call        InitGDT

    cli	                                    ; clear interrupts
	mov	        eax, cr0                    ; set bit 0 in cr0--enter pmode
	or	        eax, 1
	mov	        cr0, eax
    jmp         CODE_SEG:PMMODE




[bits 32]
PMMODE:
    mov	    ax, DATA_SEG		; set data segments to data selector (0x10)
	mov	    ds, ax
	mov	    ss, ax
	mov	    es, ax
	mov	    esp, 8000h		; stack begins from 90000h

	call	InitVideo

	call	InitPaging

    mov     ebx,LOGO
    call    print_string_pm

    jmp     $

InitVideo:

	pusha
	cld
	mov	edi, 0xb8000
	mov	cx, 2000
	mov	ah, 14
	mov	al, ' '	
	rep	stosw
	popa
	ret

VIDEO_MEMORY equ 0xb8000
WHITE_ON_BLACK equ 0x0A ; the color byte for each character

print_string_pm:
    pusha
    mov edx, VIDEO_MEMORY

print_string_pm_loop:
    mov al, [ebx] ; [ebx] is the address of our character
    mov ah, WHITE_ON_BLACK

    cmp al, 0 ; check if end of string
    je print_string_pm_done

    mov [edx], ax ; store character + attribute in video memory
    add ebx, 1 ; next char
    add edx, 2 ; next video memory position

    jmp print_string_pm_loop

print_string_pm_done:
    popa
    ret


; CR3寄存器结构; 拷贝自intel SDM
;[Table 4-3.  Use of CR3 with 32-Bit Paging]
;-Bit     Contents-
; 2:0     Ignored		低三位忽略, 填0
; 3 (PWT) Page-level write-through;  (see Section 4.9)	 控制内存缓存, 写入内存的时候, 除了写缓存, 同时也会写内存
; 4 (PCD) Page-level cache disable;  (see Section 4.9)   控制内存缓存, 禁用缓存
; 11:5    Ignored		忽略
; 31:12   Physical address of the 4-KByte aligned page directory used for linear-address translation  页目录的物理地址
; 63:32   Ignored (these bits exist only on processors supporting the Intel-64 architecture) x64用

;[Table 4-5.  Format of a 32-Bit Page-Directory Entry that References a Page Table]
;-Bit         Contents-
; 0 (P)       Present; must be 1 to reference a page table	表示是否在物理内存
; 1 (R/W)     Read/write; if 0, writes may not be allowed to the 4-MByte region controlled by this entry (see Section 4.6)	读写权限
; 2 (U/S)     User/supervisor; if 0, user-mode accesses are not allowed to the 4-MByte region controlled by this entry (see Section 4.6)	权限, 控制用户态能否访问
; 3 (PWT)     Page-level write-through; (see Section 4.9)	缓存
; 4 (PCD)     Page-level cache disable; (see Section 4.9) 	缓存
; 5 (A)       Accessed; indicates whether this entry has been used for linear-address translation (see Section 4.8) 是否已访问
; 6           Ignored
; 7 (PS)      If CR4.PSE = 1, must be 0 (otherwise, this entry maps a 4-MByte page; see Table 4-4); otherwise, ignored	4K/4M
; 11:8        Ignored
; 31:12       Physical address of 4-KByte aligned page table referenced by this entry	物理地址

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

; page directory table
%define		PAGE_DIR_CR3		0x80000

; 0th page table. Address must be 4KB aligned
%define		PAGE_TABLE_0		0x81000

; 768th page table. Address must be 4KB aligned
%define		PAGE_TABLE_768		0x82000

; each page table has 1024 entries
%define		PAGE_TABLE_ENTRIES	1024

; attributes (page is present;page is writable; supervisor mode)
%define		PRIV				3

;****************************************
;	Enable Paging
;****************************************

InitPaging:
	pusha										; save stack frame

	;------------------------------------------
	;	idenitity map 1st page table (4MB)
	;------------------------------------------

	mov		eax, PAGE_TABLE_0					; first page table
	mov		ebx, 0x0 | PRIV						; starting physical address of page
	mov		ecx, PAGE_TABLE_ENTRIES				; for every page in table...
.loop:
	mov		dword [eax], ebx					; write the entry
	add		eax, 4								; go to next page entry in table (Each entry is 4 bytes)
	add		ebx, 4096							; go to next page address (Each page is 4Kb)
	loop	.loop								; go to next entry

	;------------------------------------------
	;	map the 768th table to physical addr 1MB
	;	the 768th table starts the 3gb virtual address
	;------------------------------------------
 
	mov		eax, PAGE_TABLE_768				; first page table
	mov		ebx, 0x100000 | PRIV			; starting physical address of page
	mov		ecx, PAGE_TABLE_ENTRIES			; for every page in table...
.loop2:
	mov		dword [eax], ebx				; write the entry
	add		eax, 4							; go to next page entry in table (Each entry is 4 bytes)
	add		ebx, 4096						; go to next page address (Each page is 4Kb)
	loop	.loop2							; go to next entry

	;------------------------------------------
	;	set up the entries in the directory table
	;------------------------------------------

	mov		eax, PAGE_TABLE_0 | PRIV			; 1st table is directory entry 0
	mov		dword [PAGE_DIR_CR3], eax

	mov		eax, PAGE_TABLE_768 | PRIV			; 768th entry in directory table
	mov		dword [PAGE_DIR_CR3+(768*4)], eax

	;------------------------------------------
	;	install directory table
	;------------------------------------------

	mov		eax, PAGE_DIR_CR3
	mov		cr3, eax

	;------------------------------------------
	;	enable paging
	;------------------------------------------

	mov		eax, cr0
	or		eax, 0x80000000
	mov		cr0, eax



	popa
	ret

times   1048576-($-$$)  db  0xcc 