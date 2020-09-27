
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

mov				[BootDriveNum], dl
jmp start

BootDriveNum	db 0
LOGO        	db "loader!!", 0
IOErrorLBA     	db "Disk IO Failed:Loader LBA", 0xd , 0xa , 0
IOError     	db "Disk IO Failed:Loader CHS", 0xd , 0xa , 0


gdtinfo_unrealmode:
   dw gdtinfo_unrealmode_end - gdt_null_seg - 1     ;last byte in table
   dd gdt_null_seg                                  ;start of table
 
gdt_null_seg    dd 0,0                             ; entry 0 is always unused
gdt_data_seg    db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
gdtinfo_unrealmode_end:

start:
    xor         ax, ax
    mov         ds, ax
    mov         ss, ax
    mov         es, ax
    mov         sp, 0x8000
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
    lea         edi, [ edi + KERNEL_PHY_BASE + KERNEL_CODE_OFFSET ]
    mov		    ecx, 512
    a32         rep	 movsb                   ; copy image to its protected mode address
    pop         ecx

    loop        LOOP_READ_KERNEL

	push        LOGO
    call        PrintString
    ;call        EnableA20
    call        InitGDT

    cli	                                    ; clear interrupts
	mov	        eax, cr0                    ; set bit 0 in cr0--enter pmode
	or	        eax, 1
	mov	        cr0, eax
    jmp         CODE_SEG:PMENTRY

[bits 32]

KERNEL_VIR_BASE         equ 0XC0000400
KERNEL_SIZE		        equ (1024*1024)
PM_MODE_STACK	        equ 8000h
KERNEL_SECTOR_LBA_COUNT equ (KERNEL_SIZE/512)
KERNEL_SECTOR_LBA_BEGIN equ 8
KERNEL_PHY_BASE         equ 0x100000
KERNEL_CODE_OFFSET      equ 0x400

PMENTRY:
    mov	        ax, DATA_SEG		; set data segments to data selector (0x10)
	mov	        ds, ax
	mov	        ss, ax
	mov	        es, ax
	mov	        esp, PM_MODE_STACK			; stack begins from 90000h

	call	    InitVideo

	call	    InitPaging

    mov         ebx, LOGO
    call        print_string_pm


	mov         eax, KERNEL_VIR_BASE
	jmp         eax

    jmp         $

InitVideo:  

	pusha   
	cld 
	mov	        edi, VIDEO_MEMORY
	mov	        cx, 2000
	mov	        ah, 14
	mov	        al, ' '	
	rep	        stosw
	popa
	ret

VIDEO_MEMORY    equ 0xb8000
WHITE_ON_BLACK  equ 0x0A ; the color byte for each character

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

; 768th page table. Address must be 4KB aligned
%define		PAGE_TABLE_768		0x92000

; each page table has 1024 entries
%define		PAGE_TABLE_ENTRIES	1024

; attributes (page is present;page is writable; supervisor mode)
%define		PRIV				3

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

	;------------------------------------------
	;	map the 768th table to physical addr 1MB
	;	the 768th table starts the 3gb virtual address
	;------------------------------------------
    
	mov		    eax, PAGE_TABLE_768				; first page table
	mov		    ebx, KERNEL_PHY_BASE | PRIV			; starting physical address of page
	mov		    ecx, PAGE_TABLE_ENTRIES			; for every page in table...
.loop2: 
	mov		    dword [eax], ebx				; write the entry
	add		    eax, 4							; go to next page entry in table (Each entry is 4 bytes)
	add		    ebx, PAGES_IZE  				; go to next page address (Each page is 4Kb)
	loop	    .loop2							; go to next entry

	;------------------------------------------
	;	set up the entries in the directory table
	;------------------------------------------

	mov		    eax, PAGE_TABLE_0 | PRIV			; 1st table is directory entry 0
	mov		    dword [PAGE_DIR_CR3], eax

	mov		    eax, PAGE_TABLE_768 | PRIV			; 768th entry in directory table
	mov		    dword [PAGE_DIR_CR3+(768*4)], eax

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
    ret     2

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



times   3584-($-$$)  db  0xcc



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