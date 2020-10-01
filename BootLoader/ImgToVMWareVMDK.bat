@echo off
del /Q ..\bin\bootimg.vmdk
c:\qemu\qemu-img.exe convert -f raw -O vmdk ..\bin\BootImg.img ..\bin\BootImg.vmdk
pause