    global _main
    extern  _GetStdHandle@4
    extern  _WriteFile@20
    extern  _ExitProcess@4

    section .text
_main:
    ; DWORD  bytes;    
    mov     ebp, esp
    sub     esp, 4

    mov     ecx, message
    mov     edx, (message_end - message)
    call    printIt

    

    ; ExitProcess(0)
    push    0
    call    _ExitProcess@4

    ; never here
    hlt

; Arguments:
; ecx - pointer to string
; edx - length of string
printIt:
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
    push    edx
    push    ecx
    push    ebx
    call    _WriteFile@20

    add     esp, 4
    ret
message:
    db      'Hello, World', 10
message_end:
