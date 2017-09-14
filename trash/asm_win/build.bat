set orig_dir=%CD%
call vcvars32.bat
cd /d %orig_dir%
nasm -fwin32 %1.asm
link /subsystem:console /nodefaultlib /entry:main %1.obj kernel32.lib
