
; [[内存布局]]

; ;实模式可用区域:
; ; start         end         size            description type
; ; 0x00000500    0x00007BFF  ~30 KiB         Conventional memory usable memory   (这一段可用)
; ; 0x00007E00    0x0007FFFF  480.5 KiB       Conventional memory   (这一段可用)
; ; 0x00080000    0x0009FFFF  128 KiB         EBDA (Extended BIOS Data Area)  partially used by the EBDA  (这一段可用, 但具体多少取决于BDA:413)

; BootSector使用:
; 加载在 0x7C00 , 占512字节
; 使用0x8000作为栈

; KernelLoader:
; 加载在0x8000 (由bootSector从磁盘读取, 然后跳过来执行)
; 0x500作为加载内核读盘的buffer;
; 0XC0000400为内核加载虚拟地址
; 8000h作为栈

[bits 16]

;//实模式int13 读盘缓冲区
INT13_RM_MODE_BUFFER equ 0x500

org 0x8000
    xchg        bx, bx
    jmp         start

start:
    xor         ax, ax
    mov         ds, ax
    mov         ss, ax
    mov         es, ax
    mov         fs, ax
    mov         sp, 0x8000
    mov			[BootDriveNum], dl
    push        LOGO
    call        PrintString

    push        ds                ; save real mode
    push        es
    call        EnableA20
    lgdt        [gdtinfo_unrealmode]         ; load gdt register
 
    mov         eax, cr0          ; switch to pmode by
    or          al, 1                ; set pmode bit
    mov         cr0, eax
    
    jmp         $+2                ; tell 386/486 to not crash
    
    mov         bx, 0x08          ; select descriptor 1
    mov         ds, bx            ; 8h = 1000b
    mov         es, bx
    
    and         al, 0xFE            ; back to realmode
    mov         cr0, eax          ; by toggling bit again
    pop         es
    pop         ds                 ; get back old segment
    
    push        LOGO
    call        PrintString

    mov         ecx, KERNEL_SECTOR_LBA_COUNT
LOOP_READ_KERNEL:
 
	mov			dl,  [BootDriveNum]
	mov			bx,  INT13_RM_MODE_BUFFER			;buffer
    mov         eax, KERNEL_SECTOR_LBA_COUNT
    sub         eax, ecx
    add         eax, KERNEL_SECTOR_LBA_BEGIN
    
    push        ecx
	
    mov			cx,  1				;sector count;
	call		ReadSectorLBA

    pop         ecx
    push        ecx
    cld
    mov    	    esi, INT13_RM_MODE_BUFFER
    mov         edi, KERNEL_SECTOR_LBA_COUNT
    sub         edi, ecx
    mov         eax, 512
    mul         edi
    mov         edi, EAX
    lea         edi, [ edi + KERNEL_PHY_BASE ]
    mov		    ecx, 512
    a32         rep	 movsb                   ; copy image to its protected mode address
    pop         ecx

    loop        LOOP_READ_KERNEL

	push        LOGO
    call        PrintString
    call        EnableA20
    call        InitGDT

    cli	                                    ; clear interrupts
	mov	        eax, cr0                    ; set bit 0 in cr0--enter pmode
	or	        eax, 1
	mov	        cr0, eax
    jmp         CODE_SEG:PMENTRY

[bits 32]

KERNEL_VIR_BASE         equ 0X80000000
KERNEL_SIZE		        equ (1024*1024)
PM_MODE_STACK	        equ 80000h
KERNEL_SECTOR_LBA_COUNT equ (KERNEL_SIZE/512)
KERNEL_SECTOR_LBA_BEGIN equ 8
KERNEL_PHY_BASE         equ 0x1000000

PMENTRY:
    mov	        ax, DATA_SEG		; set data segments to data selector (0x10)
	mov	        ds, ax
	mov	        ss, ax
	mov	        es, ax
	mov	        esp, PM_MODE_STACK			; stack begins from 90000h
    ;jmp         $
	call	    InitVideo

	call	    InitPaging
    
    mov         ebx, LOGO
    call        print_string_pm

    xchg        bx, bx
	push        KERNEL_VIR_BASE
    call        GetPEEntry
    cmp         eax, 0
    je          ERROR_KERNEL_ENTRY
    push        int32
    call        eax

ERROR_KERNEL_ENTRY:
    mov         ebx, ERROR_ENTRY
    call        print_string_pm
    jmp         $

InitVideo:  

	pusha   
	cld 
	mov	        edi, VIDEO_MEMORY
	mov	        cx, 2000
	mov	        ah, 0x1e
	mov	        al, ' '	
	rep	        stosw
	popa
	ret

VIDEO_MEMORY    equ 0xb8000
WHITE_ON_BLACK  equ 0x1e ; the color byte for each character

print_string_pm:
    pusha
    mov         edx, VIDEO_MEMORY

print_string_pm_loop:
    mov         al, [ebx] ; [ebx] is the address of our character
    mov         ah, WHITE_ON_BLACK

    cmp         al, 0 ; check if end of string
    je          print_string_pm_done

    mov         [edx], ax ; store character + attribute in video memory
    add         ebx, 1 ; next char
    add         edx, 2 ; next video memory position

    jmp         print_string_pm_loop

print_string_pm_done:
    popa
    ret

; page directory table
%define		PAGE_DIR_CR3		0x90000

; 0th page table. Address must be 4KB aligned
%define		PAGE_TABLE_0		0x91000
%define		PAGE_TABLE_1		0x92000

; 768th page table. Address must be 4KB aligned
%define		PAGE_TABLE_512		0x93000

; each page table has 1024 entries
%define		PAGE_TABLE_ENTRIES	1024

; attributes (page is present;page is writable; supervisor mode)
%define		PRIV				7

PAGES_IZE       equ     4096
;****************************************
;	Enable Paging
;****************************************

InitPaging:
	pusha										; save stack frame

	;------------------------------------------
	;	idenitity map 1st page table (4MB)
	;------------------------------------------

	mov		    eax, PAGE_TABLE_0					; first page table
	mov		    ebx, 0x0 | PRIV						; starting physical address of page
	mov		    ecx, PAGE_TABLE_ENTRIES				; for every page in table...
.loop:  
	mov		    dword [eax], ebx					; write the entry
	add		    eax, 4								; go to next page entry in table (Each entry is 4 bytes)
	add		    ebx, PAGES_IZE						; go to next page address (Each page is 4Kb)
	loop	    .loop								; go to next entry


	mov		    eax, PAGE_TABLE_1				    ; first page table
	mov		    ebx, 0x400000 | 7			        ; starting physical address of page
	mov		    ecx, PAGE_TABLE_ENTRIES			    ; for every page in table...
.loop2: 
	mov		    dword [eax], ebx				    ; write the entry
	add		    eax, 4							    ; go to next page entry in table (Each entry is 4 bytes)
	add		    ebx, PAGES_IZE  				    ; go to next page address (Each page is 4Kb)
	loop	    .loop2							    ; go to next entry

	;------------------------------------------
	;	map the 768th table to physical addr 1MB
	;	the 768th table starts the 3gb virtual address
	;------------------------------------------
    
	mov		    eax, PAGE_TABLE_512				    ; first page table
	mov		    ebx, KERNEL_PHY_BASE | PRIV			; starting physical address of page
	mov		    ecx, PAGE_TABLE_ENTRIES			    ; for every page in table...
.loop3: 
	mov		    dword [eax], ebx				    ; write the entry
	add		    eax, 4							    ; go to next page entry in table (Each entry is 4 bytes)
	add		    ebx, PAGES_IZE  				    ; go to next page address (Each page is 4Kb)
	loop	    .loop3							    ; go to next entry



	;------------------------------------------
	;	set up the entries in the directory table
	;------------------------------------------

	mov		    eax, PAGE_TABLE_0 | PRIV			; 1st table is directory entry 0
	mov		    dword [PAGE_DIR_CR3], eax

	mov		    eax, PAGE_TABLE_1 | 7			; 1st table is directory entry 0
	mov		    dword [PAGE_DIR_CR3 + 1*4], eax

	mov		    eax, PAGE_TABLE_512 | PRIV			; 768th entry in directory table
	mov		    dword [PAGE_DIR_CR3 + (512*4)], eax

	;------------------------------------------
	;	install directory table
	;------------------------------------------

	mov		    eax, PAGE_DIR_CR3
	mov		    cr3, eax

	;------------------------------------------
	;	enable paging
	;------------------------------------------

	mov		    eax, cr0
	or		    eax, 0x80000000
	mov		    cr0, eax

	popa
	ret

GetPEEntry:
    push        ebp  
    mov         ebp, esp  
    sub         esp, 48h  
    push        ebx  
    push        esi  
    push        edi  

    mov         eax, dword [ebp + 8]
    mov         dword [ebp - 4], eax
    mov         eax, dword [ebp - 4]  
    movzx       ecx, word [eax]

    cmp         ecx, 5A4Dh  
    jne         ERROR_RET  

    mov         eax, dword [ebp - 4]  
    mov         ecx, dword [ebp - 4]  
    add         ecx, dword [eax + 3Ch]  
    mov         dword [ebp - 8], ecx
    mov         eax, dword [ebp - 8]  

    cmp         dword [eax], 4550h  
    jne         ERROR_RET  

    mov         eax, dword [ebp - 8]  
    mov         ecx, dword [ebp + 8]  
    add         ecx, dword [eax + 28h]  
    mov         eax, ecx  
    jmp         RET  

ERROR_RET:
    xor         eax, eax

RET:
    pop         edi  
    pop         esi  
    pop         ebx  
    mov         esp, ebp  
    pop         ebp  
    ret         4  

[bits 16]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads a sector using BIOS Int 13h fn 42h ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  EAX 		= LBA                   ;;
;;         CX    	= sector count          ;;
;;         ES:BX 	-> buffer address       ;;
;; Output: CF = 1 if error                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReadSectorLBA:
        pushad

ReadSectorLBANext:
        pusha

        push    byte 0
        push    byte 0 ; 32-bit LBA only: up to 2TB disks
        push    eax
        push    es
        push    bx
        push    byte 1 ; sector count word = 1
        push    byte 16 ; packet size byte = 16, reserved byte = 0

        mov     ah, 42h
        ;mov     dl, [bsDriveNumber]
        mov     si, sp
        push    ss
        pop     ds
        int     13h
        push    cs
        pop     ds

        jc      short ErrRead
        add     sp, 16 ; the two instructions are swapped so as not to overwrite carry flag

        popa
        dec     cx
        jz      ReadSectorLBADone2      ; last sector

        add     bx, 512 ; adjust offset for next sector
        add     eax, byte 1             ; adjust LBA for next sector
        jmp     short ReadSectorLBANext

ReadSectorLBADone2:
        popad
        ret

ErrRead:
		push IOErrorLBA
		call PrintString
		jmp $


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
    ret

PrintChar:
    push    bp
    mov     bp, sp

    mov     al, byte [bp + 4]
    mov     ah, 0xe
    int     0x10
    
    mov     sp, bp
    pop     bp
    ret     2

PrintString:
    push    bp
    mov     bp, sp
    push    si
    mov     si, [bp + 4]
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
    mov     sp, bp
    pop     bp
    ret     2;

; Input: 	ax - LBA value
; Output: 	ax - Sector
;	  		bx - Head	
;	  		cx - Cylinder

LBAToCHS:
	PUSH 	dx							; Save the value in dx
	XOR 	dx,dx						; Zero dx
	MOV 	bx, 63						; Move into place STP (LBA all ready in place)
	DIV 	bx							; Make the divide (ax/bx -> ax,dx)
	inc 	dx							; Add one to the remainder (sector value)
	push 	dx						    ; Save the sector value on the stack

	XOR 	dx,dx						; Zero dx
	MOV 	bx, 16						; Move NumHeads into place (NumTracks all ready in place)
	DIV 	bx							; Make the divide (ax/bx -> ax,dx)

	MOV 	cx,ax						; Move ax to cx (Cylinder)
	MOV 	bx,dx						; Move dx to bx (Head)
	POP 	ax							; Take the last value entered on the stack off.
										; It doesn't need to go into the same register.
										; (Sector)
	POP 	dx						    ; Restore dx, just in case something important was
										; originally in there before running this.
	RET								; Return to the main function


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


BootDriveNum	db 0
LOGO        	db "loader!!", 0
IOErrorLBA     	db "Disk IO Failed:Loader LBA", 0xd , 0xa , 0
IOError     	db "Disk IO Failed:Loader CHS", 0xd , 0xa , 0
ERROR_ENTRY     db "Error get kernel entry!", 0xd, 0xa, 0

gdtinfo_unrealmode:
   dw gdtinfo_unrealmode_end - gdt_null_seg - 1     ;last byte in table
   dd gdt_null_seg                                  ;start of table
 
gdt_null_seg    dd 0,0                             ; entry 0 is always unused
gdt_data_seg    db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
gdtinfo_unrealmode_end: 

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

; [CHS]
; Reading or writing an ATA hard disk in Real Mode with CHS addressing is precisely the same as doing the same operation on a floppy drive. Also remember that there are severe addressing limitations with CHS addressing. Typically only the first 8 GB of the media can be accessed, at most. And when you read from USB (as floppy), 1440 KB is the most.

; [Converting LBA to CHS]
; The addresses of interesting sectors on a disk are almost always calculated as LBAs, but some drives (especially USB flash drives doing floppy emulation) cannot use LBA addressing. So your code must translate the address, and use a CHS read call. This also applies if you are trying to read floppies and hard disks with the same code.

; [Quick Explanation of the Algorithm]
; You can think of a CHS address as the digits of a 3-digit number. The sectors are the low digit, the heads are the middle digit, and cylinders are the high digit. As an analogy, think of the decimal number 345. To extract the low (sectors) digit, you take the modulo with 10. 345 % 10 = 5. You also need the integer result of 345 / 10 to calculate the heads and cylinders. 345 / 10 = 34. Then %10 again gets the head, and /10 gets the cylinder. The nice thing is that all CPU chips have "div" opcodes that give you both the result and the modulus for every operation.

; [The Algorithm]
; LBA is the input value,
; Temp = LBA / (Sectors per Track)
; Sector = (LBA % (Sectors per Track)) + 1
; Head = Temp % (Number of Heads)
; Cylinder = Temp / (Number of Heads)
; Note: Always remember that Sector is 1-based, and not 0-based ... this detail causes many problems.

;=================================================================

; Getting Sectors/Track, Total Head values
; There is only one real place where you can get the "Sectors per Track" and "Number of Heads" values for the previous LBA->CHS calculation. All modern BIOSes use automatic CHS to LBA conversions internally, with internal hardcoded conversion values. They do not use the "real" CHS values that are written on the drive's label. Also, if you perhaps have a FAT formatted drive, it will claim to have "Sectors per Track" and "Number of Heads" information stored in the BPB. These values are almost always wrong.

; If the 0x80 bit is set for the BIOS drive number, then you have no real choice other than to use the INT13h AH=8 BIOS function to get the "drive geometry".

; Set AH to 8, DL to the BIOS drive number, and execute INT 0x13.
; The value returned in DH is the "Number of Heads" -1.
; AND the value returned in CL with 0x3f to get the "Sectors per Track".
; Note: INT0x13 AH=8 does not seem to work well with floppy drives, or emulated floppy drives. It may be best to use default values in that case.

; Reading sectors with a CHS address
; Cylinder = 0 to 1023 (maybe 4095), Head = 0 to 15 (maybe 254, maybe 255), Sector = 1 to 63

; Set AH = 2
; AL = total sector count (0 is illegal) -- cannot cross ES page boundary, or a cylinder boundary, and must be < 128
; CH = cylinder & 0xff
; CL = Sector | ((cylinder >> 2) & 0xC0);
; DH = Head -- may include two more cylinder bits
; ES:BX -> buffer
; Set DL = "drive number" -- typically 0x80, for the "C" drive
; Issue an INT 0x13.
; The carry flag will be set if there is any error during the read. AH should be set to 0 on success.

; To write: set AH to 3, instead.

; Note: The limitation about not crossing cylinder boundaries is very annoying, especially when combined with the 127 sector limit -- because the arithmetic for the length and "start CHS" of the next consecutive read or write gets messy. The simplest workaround is to read or write only one sector at a time in CHS mode. Not all BIOSes have these two limitations, of course, but it is necessary to program for the "lowest common denominator".

; 
; Protected Mode BIOS Call Functionailty v2.0 - by Napalm
; -------------------------------------------------------
; 
; This is code shows how its POSSIBLE to execute BIOS interrupts
; by switch out to real-mode and then back into protected mode.
; 
; If you wish to use all or part of this code you must agree
; to the license at the following URL.
; 
; License: http://creativecommons.org/licenses/by-sa/2.0/uk/
;         
; Notes: This file is in NASM syntax.
;        Turn off paging before calling these functions.
;        int32() resets all selectors.
;
; C Prototype:
;   void _cdelc int32(unsigned char intnum, regs16_t *regs);
; 
; Example of usage:
;   regs.ax = 0x0013;
;   int32(0x10, &regs);
;   memset((char *)0xA0000, 1, (320*200));
;   memset((char *)0xA0000 + (100*320+80), 14, 80);
;   regs.ax = 0x0000;
;   int32(0x16, &regs);
;   regs.ax = 0x0003;
;   int32(0x10, &regs);
; 
; 
[bits 32]
 
global int32, _int32
 
struc regs16_t
    .di resw 1
    .si resw 1
    .bp resw 1
    .sp resw 1
    .bx resw 1
    .dx resw 1
    .cx resw 1
    .ax resw 1
    .gs resw 1
    .fs resw 1
    .es resw 1
    .ds resw 1
    .ef resw 1
endstruc
 
%define INT32_BASE                             0x7C00
%define REBASE(x)                              (((x) - reloc) + INT32_BASE)
%define GDTENTRY(x)                            ((x) << 3)
%define CODE32                                 GDTENTRY(1)  ; 0x08
%define DATA32                                 GDTENTRY(2)  ; 0x10
%define CODE16                                 GDTENTRY(3)  ; 0x18
%define DATA16                                 GDTENTRY(4)  ; 0x20
%define STACK16                                (INT32_BASE - regs16_t_size)
 
 
section .text
    int32: use32                               ; by Napalm
    _int32:
        cli                                    ; disable interrupts
        ;xchg bx,bx
        mov eax, cr0
        and eax, 0x7FFFFFFF
        mov cr0, eax
        ;mov eax, 0
        ;mov cr3, eax
        ;jmp here

        here:
        pusha                                  ; save register state to 32bit stack
        mov  esi, reloc                        ; set source to code below
        mov  edi, INT32_BASE                   ; set destination to new base address
        mov  ecx, (int32_end - reloc)          ; set copy size to our codes size
        cld                                    ; clear direction flag (so we copy forward)
        rep  movsb                             ; do the actual copy (relocate code to low 16bit space)
		mov eax,INT32_BASE
        jmp eax                     ; jump to new code location
    reloc: use32                               ; by Napalm
        mov  [REBASE(stack32_ptr)], esp        ; save 32bit stack pointer
        sidt [REBASE(idt32_ptr)]               ; save 32bit idt pointer
        sgdt [REBASE(gdt32_ptr)]               ; save 32bit gdt pointer
        lgdt [REBASE(gdt16_ptr)]               ; load 16bit gdt pointer
        lea  esi, [esp+0x24]                   ; set position of intnum on 32bit stack
        lodsd                                  ; read intnum into eax
        mov  [REBASE(ib)], al                  ; set intrrupt immediate byte from our arguments 
        mov  esi, [esi]                        ; read regs pointer in esi as source
        mov  edi, STACK16                      ; set destination to 16bit stack
        mov  ecx, regs16_t_size                ; set copy size to our struct size
        mov  esp, edi                          ; save destination to as 16bit stack offset
        rep  movsb                             ; do the actual copy (32bit stack to 16bit stack)
        jmp  word CODE16:REBASE(p_mode16)      ; switch to 16bit selector (16bit protected mode)
    p_mode16: use16
        mov  ax, DATA16                        ; get our 16bit data selector
        mov  ds, ax                            ; set ds to 16bit selector
        mov  es, ax                            ; set es to 16bit selector
        mov  fs, ax                            ; set fs to 16bit selector
        mov  gs, ax                            ; set gs to 16bit selector
        mov  ss, ax                            ; set ss to 16bit selector
        mov  eax, cr0                          ; get cr0 so we can modify it
        and  al,  ~0x01                        ; mask off PE bit to turn off protected mode
        mov  cr0, eax                          ; set cr0 to result
        jmp  word 0x0000:REBASE(r_mode16)      ; finally set cs:ip to enter real-mode
    r_mode16: use16
        xor  ax, ax                            ; set ax to zero
        mov  ds, ax                            ; set ds so we can access idt16
        mov  ss, ax                            ; set ss so they the stack is valid
        lidt [REBASE(idt16_ptr)]               ; load 16bit idt
        mov  bx, 0x0870                        ; master 8 and slave 112
        call resetpic                          ; set pic's the to real-mode settings
        popa                                   ; load general purpose registers from 16bit stack
        pop  gs                                ; load gs from 16bit stack
        pop  fs                                ; load fs from 16bit stack
        pop  es                                ; load es from 16bit stack
        pop  ds                                ; load ds from 16bit stack
        sti                                    ; enable interrupts
        db 0xCD                                ; opcode of INT instruction with immediate byte
    ib: db 0x00
        cli                                    ; disable interrupts
        xor  sp, sp                            ; zero sp so we can reuse it
        mov  ss, sp                            ; set ss so the stack is valid
        mov  sp, INT32_BASE                    ; set correct stack position so we can copy back
        pushf                                  ; save eflags to 16bit stack
        push ds                                ; save ds to 16bit stack
        push es                                ; save es to 16bit stack
        push fs                                ; save fs to 16bit stack
        push gs                                ; save gs to 16bit stack
        pusha                                  ; save general purpose registers to 16bit stack
        mov  bx, 0x2028                        ; master 32 and slave 40
        call resetpic                          ; restore the pic's to protected mode settings
        mov  eax, cr0                          ; get cr0 so we can modify it
        inc  eax                               ; set PE bit to turn on protected mode
        mov  cr0, eax                          ; set cr0 to result
        jmp  dword CODE32:REBASE(p_mode32)     ; switch to 32bit selector (32bit protected mode)
    p_mode32: use32
        mov  ax, DATA32                        ; get our 32bit data selector
        mov  ds, ax                            ; reset ds selector
        mov  es, ax                            ; reset es selector
        mov  fs, ax                            ; reset fs selector
        mov  gs, ax                            ; reset gs selector
        mov  ss, ax                            ; reset ss selector
        lgdt [REBASE(gdt32_ptr)]               ; restore 32bit gdt pointer
        lidt [REBASE(idt32_ptr)]               ; restore 32bit idt pointer
        mov  esp, [REBASE(stack32_ptr)]        ; restore 32bit stack pointer
        mov  esi, STACK16                      ; set copy source to 16bit stack
        lea  edi, [esp+0x28]                   ; set position of regs pointer on 32bit stack
        mov  edi, [edi]                        ; use regs pointer in edi as copy destination
        mov  ecx, regs16_t_size                ; set copy size to our struct size
        cld                                    ; clear direction flag (so we copy forward)
        rep  movsb                             ; do the actual copy (16bit stack to 32bit stack)
        popa                                   ; restore registers
    
        mov		    eax, cr0
        or          eax, 0x80000000
        mov		    cr0, eax
        mov         eax, 0x90000
        mov         cr3, eax
        sti                                    ; enable interrupts
        ret                                    ; return to caller
         
    resetpic:                                  ; reset's 8259 master and slave pic vectors
        push ax                                ; expects bh = master vector, bl = slave vector
        mov  al, 0x11                          ; 0x11 = ICW1_INIT | ICW1_ICW4
        out  0x20, al                          ; send ICW1 to master pic
        out  0xA0, al                          ; send ICW1 to slave pic
        mov  al, bh                            ; get master pic vector param
        out  0x21, al                          ; send ICW2 aka vector to master pic
        mov  al, bl                            ; get slave pic vector param
        out  0xA1, al                          ; send ICW2 aka vector to slave pic
        mov  al, 0x04                          ; 0x04 = set slave to IRQ2
        out  0x21, al                          ; send ICW3 to master pic
        shr  al, 1                             ; 0x02 = tell slave its on IRQ2 of master
        out  0xA1, al                          ; send ICW3 to slave pic
        shr  al, 1                             ; 0x01 = ICW4_8086
        out  0x21, al                          ; send ICW4 to master pic
        out  0xA1, al                          ; send ICW4 to slave pic
        pop  ax                                ; restore ax from stack
        ret                                    ; return to caller
         
    stack32_ptr:                               ; address in 32bit stack after we
        dd 0x00000000                          ;   save all general purpose registers
         
    idt32_ptr:                                 ; IDT table pointer for 32bit access
        dw 0x0000                              ; table limit (size)
        dd 0x00000000                          ; table base address
         
    gdt32_ptr:                                 ; GDT table pointer for 32bit access
        dw 0x0000                              ; table limit (size)
        dd 0x00000000                          ; table base address
         
    idt16_ptr:                                 ; IDT table pointer for 16bit access
        dw 0x03FF                              ; table limit (size)
        dd 0x00000000                          ; table base address
         
    gdt16_base:                                ; GDT descriptor table
        .null:                                 ; 0x00 - null segment descriptor
            dd 0x00000000                      ; must be left zero'd
            dd 0x00000000                      ; must be left zero'd
             
        .code32:                               ; 0x01 - 32bit code segment descriptor 0xFFFFFFFF
            dw 0xFFFF                          ; limit  0:15
            dw 0x0000                          ; base   0:15
            db 0x00                            ; base  16:23
            db 0x9A                            ; present, iopl/0, code, execute/read
            db 0xCF                            ; 4Kbyte granularity, 32bit selector; limit 16:19
            db 0x00                            ; base  24:31
             
        .data32:                               ; 0x02 - 32bit data segment descriptor 0xFFFFFFFF
            dw 0xFFFF                          ; limit  0:15
            dw 0x0000                          ; base   0:15
            db 0x00                            ; base  16:23
            db 0x92                            ; present, iopl/0, data, read/write
            db 0xCF                            ; 4Kbyte granularity, 32bit selector; limit 16:19
            db 0x00                            ; base  24:31
             
        .code16:                               ; 0x03 - 16bit code segment descriptor 0x000FFFFF
            dw 0xFFFF                          ; limit  0:15
            dw 0x0000                          ; base   0:15
            db 0x00                            ; base  16:23
            db 0x9A                            ; present, iopl/0, code, execute/read
            db 0x0F                            ; 1Byte granularity, 16bit selector; limit 16:19
            db 0x00                            ; base  24:31
             
        .data16:                               ; 0x04 - 16bit data segment descriptor 0x000FFFFF
            dw 0xFFFF                          ; limit  0:15
            dw 0x0000                          ; base   0:15
            db 0x00                            ; base  16:23
            db 0x92                            ; present, iopl/0, data, read/write
            db 0x0F                            ; 1Byte granularity, 16bit selector; limit 16:19
            db 0x00                            ; base  24:31
             
    gdt16_ptr:                                 ; GDT table pointer for 16bit access
        dw gdt16_ptr - gdt16_base - 1          ; table limit (size)
        dd gdt16_base                          ; table base address
         
    int32_end:                                 ; end marker (so we can copy the code)

    times   3584-($-$$)  db  0xcc