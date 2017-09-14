.386
.model flat, stdcall
include kernel32.inc
includelib kernel32.lib
include user32.inc
includelib user32.lib

.data
title db 'Win32', 0
msg db 'Hello World', 0

.code

Main:
push 0            ; uType = MB_OK
push offset title ; LPCSTR lpCaption
push offset msg   ; LPCSTR lpText
push 0            ; hWnd = HWND_DESKTOP
call MessageBoxA
push eax          ; uExitCode = MessageBox(...)
call ExitProcess

End Main

;---ASM Hello World Win64 MessageBox

extrn MessageBoxA: PROC
extrn ExitProcess: PROC

.data
title db 'Win64', 0
msg db 'Hello World!', 0

.code
main proc
  sub rsp, 28h  
  mov rcx, 0       ; hWnd = HWND_DESKTOP
  lea rdx, msg     ; LPCSTR lpText
  lea r8,  title   ; LPCSTR lpCaption
  mov r9d, 0       ; uType = MB_OK
  call MessageBoxA
  add rsp, 28h  
  mov ecx, eax     ; uExitCode = MessageBox(...)
  call ExitProcess
main endp

End
