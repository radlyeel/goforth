; GoForth source code
;    (c)  Daryl Lee, 2016
;    (cf) http://www.bradrodriguez.com/papers/moving1.htm
;         Referred to herein as "Moving Forth"
;
; Environment Controls
%define OSX                     ; OSX, LINUX, or WINDOWS
%define CELL_WIDTH 8

; FORTH registers (See Moving Forth)
%define W    r14                ; CFA of current word
%define IP   r15                ; Forth Interpreter Pointer
                                ; Address of XT field of next word to execute
%define X    r12                ; X register (pointer to executable code
%define SP   rsp                ; Forth Data Stack Pointer
%define FRSP r13                ; Forth Return Stack Pointer (can't call it RSP)

; Given these register assignments, we can define some helpful macros:
%macro  PUSHRSP 1
        sub FRSP, CELL_WIDTH    ; Make room for new item
        mov [FRSP], %1          ; Put the requested item there
%endmacro
%macro  POPRSP 1
        mov [FRSP], %1          ; Get the requested item
        add FRSP, CELL_WIDTH
%endmacro
        
; Dictionary entry structure (CELL_WIDTH bits wide)

; Quote from Moving Forth: "The word size used by Forth is not necessarily 
;     the same as that of the CPU."
;    +----------------------------+
;    |  Flags       |  Name Length|   See below for flags
;    +----------------------------+
;    |  Name, zero-padded to cell |
;    +----------------------------+
;    |  ..boundary, first 8 chars |
;    +----------------------------+
;    |  Link to previous entry    |
;    +----------------------------+
;    |  Code Field                |   Pointer to executable code; see below
;    +----------------------------+
;    |  Parameter Field           |   
;    +----------------------------+
;    |  ..unknown length          |                     |    
;    +----------------------------+
; 
; Flags:
;   b0 -- Invisible; word cannot be found in dictionary search
;   b1 -- Immediate; word executes during compilation
%define INVISIBLE 0x01
%define IMMEDIATE 0x02

; This design uses Indirect Threaded Code (ITC).
; Execution Token:
;   Always points to executable assembler code
;     For primitives, points to implementation code
;     For high level code, points to ENTER

; Dictionary macro for low-level words (implemented in assembler
; parameters: label flags len name link
%macro  code 5
%1:
        dq %2<<(CELL_WIDTH/2)+%3
        db %4
        align CELL_WIDTH
        dq %5
        dq start.do_%1
%endmacro

; Dictionary macro for high-level words (implemented in Forth)
; parameters: label flags len name link
%macro  enter 5
%1:
        dq %2
        dq %3
        db %4
        align CELL_WIDTH
        dq %5
        dq start.do_enter
%endmacro

; Macro for finishing up a low-level word
%macro  next 0
        POPRSP rax
        jmp (rax)
%endmacro
;--------- End of Macros ------------
;--------- Data Segment  ------------
        segment .data
EOD:    dd  0
        code plus, 0, 1, '+', EOD
        code minus, 0, 1, '-', plus
        code nother, 0, 1, '?', minus
        enter nil, 0, 3, 'nil', nother
        dw   plus, minus, start.do_exit

; End of initial dictionary

;--------- Text Segment -------------
; Start of implementation code
        segment .text
        global start
start:
        jmp go
        
; The Forth Interpreter
.do_enter:
        PUSHRSP IP
        mov IP,(W)
        add IP, CELL_WIDTH
.do_next:
        mov W, (IP)
        add IP, CELL_WIDTH
        mov X,(W)
        jmp (X)

.do_exit:
        POPRSP IP
        jmp .do_next
        
       
.do_minus:
        pop rbx
        pop rax
        sub rax,rbx
        push rax
        next

.do_plus:
        pop rax
        pop rbx
        add rax,rbx
        push rax
        next
        
.do_nother:
        pop rax
        pop rbx
        add rax,rbx
        push rax
        next
.do_term:

go:
        mov  IP, plus-$         ; Initialize IP
        PUSHRSP IP
        next
        
%ifdef  OSX
        mov eax,0x2000001       ; exit syscall number for OS X
        mov edi,0               ; the status value to return 
        syscall                 ; execute a system call 
%elif   LINUX
        mov eax,60              ; exit syscall number for Linux
        mov edi,0               ; the status value to return 
        syscall                 ; execute a system call 
%else
        mov eax,60              ; exit syscall number for Windows
        mov edi,0               ; the status value to return 
        syscall                 ; execute a system call 
%endif
.end:
        end
