@echo off
del /Q ..\bin\bootimg.vdi
c:\qemu\qemu-img.exe convert -f raw -O vdi ..\bin\BootImg.img ..\bin\BootImg.vdi
pause