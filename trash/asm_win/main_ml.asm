.386
.model flat

extrn  _GetStdHandle@4:PROC
extrn  _WriteFile@20:PROC
extrn  _ExitProcess@4:PROC

.data
message     db  'Hello, World', 10
msg_len     equ $-message

.code
_main PROC
    ; DWORD  bytes;    
    mov     ebp, esp
    sub     esp, 4

    ; hStdOut = GetstdHandle( STD_OUTPUT_HANDLE)
    push    -11
    call    _GetStdHandle@4
    mov     ebx, eax    

    ; WriteFile( hstdOut, message, length(message), &bytes, 0);
    push    0
    lea     eax, [ebp-4]
    push    eax
    push    offset message
    push    5
    push    ebx
    call    _WriteFile@20

    ; ExitProcess(0)
    push    0
    call    _ExitProcess@4

    ; never here
    hlt
_main ENDP
END _main
