
; compile: nasm -f bin BootSector.asm -o BootSector.bin
; testrun: qemu-system-x86_64.exe ./BootSector.bin

[bits 16]

org 0x7c00

start:
    cli
    mov     sp, 0x8000
    sti
    call    PrintEndLine
    
    push    LogoStr
    call    PrintString

    jmp $


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
    xor     ax, ax

    REPEAT:

        mov     al, [si]
        cmp     al, 0
        jz      END

        push    ax
        call    PrintChar
        inc     si

        jmp     REPEAT

    END:

        call    PrintEndLine
    pop     si
    ret     2;

LogoStr db "................", 0xd, 0xa,  0xd, 0xa, "ZhaZha BootLoader - 20200922"

times   510-($-$$) db 0       ;填充0, 凑够512字节
dw      0xAA55                   ;MBR Magic



;BIOS MemoryMap

;start	    end	        size	        description	type
;0x00000000	0x000003FF	1 KiB	        Real Mode IVT (Interrupt Vector Table)	unusable in real mode	640 KiB RAM ("Low memory")
;0x00000400	0x000004FF	256 bytes	    BDA (BIOS data area)
;0x00000500	0x00007BFF	~30 KiB	        Conventional memory	usable memory   (这一段可用)
;0x00007C00	0x00007DFF	512 bytes	    Your OS BootSector
;0x00007E00	0x0007FFFF	480.5 KiB	    Conventional memory   (这一段可用)
;0x00080000	0x0009FFFF	128 KiB	        EBDA (Extended BIOS Data Area)	partially used by the EBDA  (这一段可用, 但具体多少取决于BDA:413)
;0x000A0000	0x000BFFFF	128 KiB	        Video display memory	hardware mapped	384 KiB System / Reserved ("Upper Memory")
;0x000C0000	0x000C7FFF	~32 KiB 	    Video BIOS	ROM and hardware mapped / Shadow RAM
;0x000C8000	0x000EFFFF	~160 KiB 	    BIOS Expansions
;0x000F0000	0x000FFFFF	64 KiB	        Motherboard BIOS


;BIOS Data Area (BDA)

;address        (size)	        description
;0x0400         (4 words)	    IO ports for COM1-COM4 serial (each address is 1 word, zero if none)
;0x0408         (3 words)	    IO ports for LPT1-LPT3 parallel (each address is 1 word, zero if none)
;0x040E         (word)	        EBDA base address >> 4 (usually!)
;0x0410         (word)	        packed bit flags for detected hardware
;0x0413         (word)	        Number of kilobytes before EBDA / unusable memory (这里决定EBDA区域有多少可用内存, 单位K)
;0x0417         (word)	        keyboard state flags
;0x041E         (32 bytes)	    keyboard buffer
;0x0449         (byte)	        Display Mode
;0x044A         (word)	        number of columns in text mode
;0x0463         (2 bytes)	    base IO port for video
;0x046C         (word)	        # of IRQ0 timer ticks since boot
;0x0475         (byte)	        # of hard disk drives detected
;0x0480         (word)	        keyboard buffer start
;0x0482         (word)	        keyboard buffer end
;0x0497         (byte)	        last keyboard LED/Shift key state