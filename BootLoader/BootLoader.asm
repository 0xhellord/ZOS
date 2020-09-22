[bits 16]

org 0x8000

jmp start

LOGO        db "loader!!",0xd,0xa,0x0
IOError     db "Disk IO Failed", 0xd , 0xa , 0

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

times   16384-($-$$)  db  0xcc 