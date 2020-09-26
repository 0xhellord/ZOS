
; compile: nasm -f bin BootSector.asm -o ./bin/BootSector.bin
;     run: qemu-system-x86_64.exe ./bin/BootSector.bin

[bits 16]

org 0x7c00

jmp start

TestMsg db "returned from loader??", 0xd, 0xa, 0
IOError db "Disk IO Failed:BootSec", 0xd , 0xa , 0
LogoStr db "................", 0xd, 0xa,  0xd, 0xa, "ZhaZha BootLoader - 20200922", 0

start:
    cli
    xor     ax, ax
    mov     ds, ax
    mov     ss, ax
    mov     es, ax
    mov     sp, 0x8000
    sti
    
    call    PrintEndLine
    
    push    LogoStr
    call    PrintString

    mov     al,0x7
    mov     cl,2
    mov     bx,0x8000
    call    ReadSector
    mov     bx,0x8000
    call    bx
    push    TestMsg
    call    PrintString
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



times   510-($-$$)  db  0xcc        ;填充, 凑够512字节
dw      0xAA55                      ;MBR Magic

; mov  ax, TestMsg
; mov  bx, PrintString
; push ax
; call bx
; ret


;times   4096        db  0xcc


; [BIOS MemoryMap]

; start         end         size            description type
; 0x00000000    0x000003FF  1 KiB           Real Mode IVT (Interrupt Vector Table)  unusable in real mode   640 KiB RAM ("Low memory")
; 0x00000400    0x000004FF  256 bytes       BDA (BIOS data area)
; 0x00000500    0x00007BFF  ~30 KiB         Conventional memory usable memory   (这一段可用)
; 0x00007C00    0x00007DFF  512 bytes       Your OS BootSector
; 0x00007E00    0x0007FFFF  480.5 KiB       Conventional memory   (这一段可用)
; 0x00080000    0x0009FFFF  128 KiB         EBDA (Extended BIOS Data Area)  partially used by the EBDA  (这一段可用, 但具体多少取决于BDA:413)
; 0x000A0000    0x000BFFFF  128 KiB         Video display memory    hardware mapped 384 KiB System / Reserved ("Upper Memory")
; 0x000C0000    0x000C7FFF  ~32 KiB         Video BIOS  ROM and hardware mapped / Shadow RAM
; 0x000C8000    0x000EFFFF  ~160 KiB        BIOS Expansions
; 0x000F0000    0x000FFFFF  64 KiB          Motherboard BIOS


; [BIOS Data Area (BDA)]

; address        (size)         description
; 0x0400         (4 words)      IO ports for COM1-COM4 serial (each address is 1 word, zero if none)
; 0x0408         (3 words)      IO ports for LPT1-LPT3 parallel (each address is 1 word, zero if none)
; 0x040E         (word)         EBDA base address >> 4 (usually!)
; 0x0410         (word)         packed bit flags for detected hardware
; 0x0413         (word)         Number of kilobytes before EBDA / unusable memory (这里决定EBDA区域有多少可用内存, 单位K)
; 0x0417         (word)         keyboard state flags
; 0x041E         (32 bytes)     keyboard buffer
; 0x0449         (byte)         Display Mode
; 0x044A         (word)         number of columns in text mode
; 0x0463         (2 bytes)      base IO port for video
; 0x046C         (word)         # of IRQ0 timer ticks since boot
; 0x0475         (byte)         # of hard disk drives detected
; 0x0480         (word)         keyboard buffer start
; 0x0482         (word)         keyboard buffer end
; 0x0497         (byte)         last keyboard LED/Shift key state


; [Read Sectors From Drive(CHS)]
; INT 13h AH=02h

; -Parameters-
; AH        02h
; AL        Sectors To Read Count
; CH        Cylinder
; CL        Sector
; DH        Head
; DL        Drive
; ES:BX Buffer Address Pointer

; -Results-
; CF        Set On Error, Clear If No Error
; AH        Return Code
; AL        Actual Sectors Read Count

; -Remarks-

; Register CX contains both the cylinder number (10 bits, possible values are 0 to 1023),
; and the sector number (6 bits, possible values are 1 to 63). 

; Cylinder and Sector bits are numbered below:

; CX =       ---CH--- ---CL---
; cylinder : 76543210 98
; sector   :            543210
; Examples of translation:

; CX := ( ( cylinder and 255 ) shl 8 ) or ( ( cylinder and 768 ) shr 2 ) or sector;
; cylinder := ( (CX and 0xFF00) shr 8 ) or ( (CX and 0xC0) shl 2)
; sector := CX and 63;
; Addressing of Buffer should guarantee that the complete buffer is inside the given segment, 
; i.e. ( BX + size_of_buffer ) <= 10000h. Otherwise the interrupt may fail with some BIOS or hardware versions.

; -Example-
; Assume you want to read 16 sectors (= 2000h bytes),
; and your buffer starts at memory address 4FF00h. 
; Utilizing memory segmentation, there are different ways to calculate the register values, e.g.:

; ES = segment         =    4F00h
; BX = offset          =  0F00h
; sum = memory address =    4FF00h
; would be a good choice because 0F00h + 2000h = 2F00h <= 10000h
; ES = segment         =    4000h
; BX = offset          =  FF00h
; sum = memory address =    4FF00h
; would not be a good choice because FF00h + 2000h = 11F00h > 10000h

; Function 02h of interrupt 13h may only read sectors of the first 16,450,560 sectors of your hard drive, 
; to read sectors beyond the 8 GB limit you should use function 42h of INT 13h Extensions. 
; Another alternate may be DOS interrupt 25h which reads sectors within a partition.


; [Write Sectors To Drive(CHS)]

; INT 13h AH=03h
; -Parameters-
; AH	03h
; AL	Sectors To Write Count
; CH	Track
; CL	Sector
; DH	Head
; DL	Drive
; ES:BX	Buffer Address Pointer

; -Results-
; CF	Set On Error, Clear If No Error
; AH	Return Code
; AL	Actual Sectors Written Count

; [Extended Read Sectors From Drive(LBA)]
; INT 13h AH=42h:

; -Parameters-
; Registers	Description
; AH	        42h = function number for extended read
; DL	        drive index (e.g. 1st HDD = 80h)
; DS:SI	    segment:offset pointer to the DAP, see below

; -DAP : Disk Address Packet-
; offset range	size	description
; 00h	                1 byte	size of DAP (set this to 10h)
; 01h	                1 byte	unused, should be zero
; 02h..03h	            2 bytes	number of sectors to be read, (some Phoenix BIOSes are limited to a maximum of 127 sectors)
; 04h..07h	            4 bytes	segment:offset pointer to the memory buffer to which sectors will be transferred (note that x86 is little-endian: if declaring the segment and offset separately, the offset must be declared before the segment)
; 08h..0Fh	            8 bytes	absolute number of the start of the sectors to be read (1st sector of drive has number 0) using logical block addressing (note that the lower half comes before the upper half)[9]

; -Results-
; Registers	    Description
; CF	        Set On Error, Clear If No Error
; AH	        Return Code

; As already stated with int 13h AH=02h, 
; care must be taken to ensure that the complete buffer is inside the given segment, i.e. ( BX + size_of_buffer ) <= 10000h

; [Extended Write Sectors to Drive]
; INT 13h AH=43h

; -Parameters-
; Registers	    Description
; AH	        43h = function number for extended write
; AL	        bit 0 = 0: close write check,bit 0 = 1: open write check,bit 1 - 7: reserved, set to 0
; DL	        drive index (e.g. 1st HDD = 80h)
; DS:SI	        segment:offset pointer to the DAP

; -Results-
; Registers	    Description
; CF	        Set On Error, Clear If No Error
; AH	        Return Code