@echo off

mkdir ..\bin > NUL

del /Q ..\bin\BootImg.img

rem 创建64M 平坦磁盘镜像
..\tool\dd if=/dev/zero of=..\bin\BootImg.img bs=1M count=64

:BUILD
cls
ECHO "按任意键编译BootSector"
pause > NUL
..\tool\nasm -f bin BootSector.asm -o ..\bin\Boot-0-Sector.bin
..\tool\nasm -f bin BootLoader.asm -o ..\bin\Boot-1-Loader.bin
rem ..\tool\objcopy -O binary ../bin/Kernel.exe ../bin/Boot-2-Kernel.bin

del ..\bin\Boot-2-Kernel.bin
copy ..\bin\Kernel.exe ..\bin\Boot-2-Kernel.bin


ECHO "按任意键启动Bochsdbg"
pause > NUL

rem 写入BootSector
..\tool\dd if=..\bin\Boot-0-Sector.bin of=..\bin\BootImg.img
rem 写入KernelLoader, 从512偏移开始写
..\tool\dd if=..\bin\Boot-1-Loader.bin of=..\bin\BootImg.img bs=512 seek=1
rem 写入Kernel, 从4K 偏移开始写
..\tool\dd if=..\bin\Boot-2-Kernel.bin of=..\bin\BootImg.img bs=512 seek=8
rem copy  /B /Y ..\bin\Boot-0-Sector.bin + ..\bin\Boot-1-Loader.bin + ..\bin\Boot-2-Kernel.bin+ ..\tool\objcopy.exe ..\bin\BootImg.img

rem C:\qemu\qemu-system-i386.exe  -drive format=raw,file=..\bin\BootImg.img
rem c:\qemu\qemu-img.exe convert -f raw -O vmdk BootImg.img BootImg.vmdk

..\tool\Bochs-2.6.11\bochsdbg.exe -f bochsrc.bxrc -q -unlock

goto BUILD