@echo off
mkdir bin > NUL
:BUILD
cls
ECHO "�����������BootSector"
pause > NUL
nasm -f bin BootSector.asm -o ./bin/Boot-0-Sector.bin
nasm -f bin BootLoader.asm -o ./bin/Boot-1-Loader.bin
ECHO "�����������Qemu����BootSector"
pause > NUL
del /Q .\bin\BootImg.img
copy  /B /Y .\bin\Boot-0-Sector.bin+.\bin\Boot-1-Loader.bin .\bin\BootImg.img
C:\qemu\qemu-system-x86_64.exe ./bin/BootImg.img
goto BUILD