@echo off
mkdir ..\bin > NUL
:BUILD
cls
ECHO "按任意键编译BootSector"
pause > NUL
..\tool\nasm -f bin BootSector.asm -o ..\bin\Boot-0-Sector.bin
..\tool\nasm -f bin BootLoader.asm -o ..\bin\Boot-1-Loader.bin
..\tool\objcopy -O binary ../bin/Kernel.exe ../bin/Boot-2-Kernel.bin
ECHO "按任意键启动Qemu运行BootSector"
pause > NUL
del /Q ..\bin\BootImg.img
..\tool\dd if=/dev/zero of=..\bin\BootImg.img bs=1M count=64
..\tool\dd if=..\bin\Boot-0-Sector.bin of=..\bin\BootImg.img
..\tool\dd if=..\bin\Boot-1-Loader.bin of=..\bin\BootImg.img bs=512 seek=1
..\tool\dd if=..\bin\Boot-2-Kernel.bin of=..\bin\BootImg.img bs=512 seek=8
rem copy  /B /Y ..\bin\Boot-0-Sector.bin + ..\bin\Boot-1-Loader.bin + ..\bin\Boot-2-Kernel.bin+ ..\tool\objcopy.exe ..\bin\BootImg.img
rem C:\qemu\qemu-system-i386.exe  -drive format=raw,file=..\bin\BootImg.img

..\tool\Bochs-2.6.11\bochsdbg.exe -f bochsrc.bxrc -q -unlock
pause
goto BUILD