@echo off
echo Compiling...

set NAME=ROMain
set AC=sjasmplus
set OBJ=obj
set REL=release
set ROM=rom

if exist %OBJ% rd /s /q %OBJ%
md %OBJ%

if exist %REL% rd /s /q %REL%
md %REL%

%AC% build_ram.a80
if errorlevel 1 pause & exit

echo Packing...
zx0 -f "%OBJ%\%NAME%.cod" "%OBJ%\%NAME%.zx0"

%AC% build_rom.a80
if errorlevel 1 pause & exit

echo Building ROM...
copy /b "%OBJ%\%NAME%.bin" + "%ROM%\trdos504t.rom" + "%ROM%\128.rom" "%REL%\%NAME%.rom" >nul
if errorlevel 1 pause & exit

echo Building ROM [RAMDISK C]...
copy /b "%OBJ%\%NAME%.bin" + "%ROM%\profi_trdos_c.rom" + "%ROM%\128.rom" "%REL%\%NAME%_ramdisk_C.rom" >nul
if errorlevel 1 pause & exit

echo Building ROM [RAMDISK D]...
copy /b "%OBJ%\%NAME%.bin" + "%ROM%\profi_trdos_d.rom" + "%ROM%\128.rom" "%REL%\%NAME%_ramdisk_D.rom" >nul
if errorlevel 1 pause & exit

echo Building ROM with Fatall [RAMDISK D]...
copy /b "%OBJ%\%NAME%.bin" + "%ROM%\profi_trdos_d.rom" + "%ROM%\fatall_disk_D.rom" + "%ROM%\48.rom" "%REL%\%NAME%_fatall_ramdisk_D.rom" >nul
if errorlevel 1 pause & exit

echo Copying changelog...
copy changelog.txt %REL%
if errorlevel 1 pause & exit

pause