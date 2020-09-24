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
WHITE_ON_BLACK equ 0x0f ; the color byte for each character

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
; page directory table
%define		PAGE_DIR			0x80000

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
	;	set up the entries in the directory table
	;------------------------------------------

	mov		eax, PAGE_TABLE_0 | PRIV			; 1st table is directory entry 0
	mov		dword [PAGE_DIR], eax

	mov		eax, PAGE_TABLE_768 | PRIV			; 768th entry in directory table
	mov		dword [PAGE_DIR+(768*4)], eax

	;------------------------------------------
	;	install directory table
	;------------------------------------------

	mov		eax, PAGE_DIR
	mov		cr3, eax

	;------------------------------------------
	;	enable paging
	;------------------------------------------

	mov		eax, cr0
	or		eax, 0x80000000
	mov		cr0, eax

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

	popa
	ret

times   1048576-($-$$)  db  0xcc 