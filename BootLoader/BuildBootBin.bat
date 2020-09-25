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
copy  /B /Y ..\bin\Boot-0-Sector.bin + ..\bin\Boot-1-Loader.bin + ..\bin\Boot-2-Kernel.bin ..\bin\BootImg.img
C:\qemu\qemu-system-x86_64.exe  -drive format=raw,file=..\bin\BootImg.img
goto BUILD