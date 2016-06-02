@ GoForth source code
@    (c)  Daryl Lee, 2016
@    (cf) http://www.bradrodriguez.com/papers/moving1.htm
@         Referred to herein as "Moving Forth"
@ Originated with YASM/ebe; migrated to GNU as for ARM implementation
@
@ Environment Controls
ARM=1                            @ OSX, LINUX, WINDOWS, ARM
CELL_WIDTH=4

@ FORTH registers (See Moving Forth)
@   W  r10   CFA of current word
@  IP  r9    Forth Interpreter Pointer
@   X  r12   X register (pointer to executable code
@  SP  r13   Forth Data Stack Pointer (same as native SP)
@ RSP  r11   Forth Return Stack Pointer

@ Given these register assignments, we can define some helpful macros:
.macro  push reg
        stmfd r13!, {\reg}
.endm
.macro  pop reg
        ldmfd r13!, {\reg}
.endm
.macro  pushrsp reg
        stmfd r11!, {\reg}
.endm
.macro  poprsp reg
        ldmfd r11!, {\reg}
.endm
        
@ Dictionary entry structure (CELL_WIDTH bits wide)

@ This design uses Indirect Threaded Code (ITC) as described in Moving Forth
@    +----------------------------+
@    |  Flags       |  Name Length|   See below for flags
@    +----------------------------+
@    |  Name, zero-padded to cell |
@    +----------------------------+
@    |  ..boundary, first 8 chars |
@    +----------------------------+
@    |  Link to previous entry    |
@    +----------------------------+
@    |  Code Field                |   Pointer to executable code@ see below
@    +----------------------------+
@    |  Parameter Field           |   
@    +----------------------------+
@    |  ..unknown length          |                     |    
@    +----------------------------+
@ 
@ Flags:
@   b0 -- Invisible@ word cannot be found in dictionary search
@   b1 -- Immediate@ word executes during compilation
INVISIBLE=0x01
IMMEDIATE=0x02


@ Dictionary macro header
.macro  dict label, flags, len, name, link
\label:
  .hword \flags
  .hword \len
  .ascii "\name"
  .iflt \len-8
   .rept (8-\len)
    .byte 0
   .endr
  .endif
  .word \link
.endm

@ Dictionary entry for primitives (implmemented in assembler)
.macro code label, flags, len, name, link
  dict \label, \flags, \len, \name, \link
  cf_\label: .word do_\label
.endm

@ Dictionary entry for high-level words (implemented in Forth)
.macro word label, flags, len, name, link
  dict \label, \flags, \len, \name, \link
  cf_\label: .word do_enter
.endm

@ Dictionary entry for built-in Forth VARIABLEs
.macro variable label, flags, len, name, link
  dict \label, \flags, \len, \name, \link
  cf_\label: .word do_variable
  .word 0
.endm

@ Dictionary entry for built-in Forth CONSTANTs
.macro constant label, flags, len, name, link, value
  dict \label, \flags, \len, \name, \link
  cf_\label: .word do_constant
  .word \value
.endm


@ Macro for finishing up a low-level word
.macro  next
        b do_next
.endm

@ OS System call
@  For codes see /usr/include/.../asm/unistd.h
@  For calling sequence see http://man7.org/linux/man-pages/man2/syscall.2.html, and use
@    the lines for arm/EABI
.macro syscall calltype
        mov r7, #\calltype
        swi #0
.endm

@--------- End of Macros ------------
@--------- Data Segment  ------------
        .data
        code star, 0, 1, *, 0
        code dup, 0, 3, dup, star
        code exit, 0, 4, exit, dup
        code bye, 0, 3, bye, exit

@ testing area; see also start:
        word square, 0, 6, square, bye
        .word   cf_dup, cf_star, cf_exit

stack_low:
        .rept 4096
        .word 0
        .endr
stack_high:
rstack_low:
        .rept 4096
        .word 0
        .endr
rstack_high:

@ End of initial dictionary

@--------- Text Segment -------------
@ Start of implementation code
        .text
.global _start
_start:
        b go
        
@ The Forth Interpreter

do_enter:                               @ Pseudocode from MF:
        pushrsp r9                      @ PUSH IP
        add r9, r10, #CELL_WIDTH        @ W+2 -> IP
        b do_next
do_exit:
        poprsp r9                       @ POP IP (from return stack)
do_next:                                @ JUMP to interpreter ("NEXT")
        ldr r10, [r9]                   @ (IP) -> W
        add r9, r9, #CELL_WIDTH         @ IP + 2 -> IP
        ldr r12, [r10]                  @ (W) -> X
        mov pc,r12                      @ JP (X)

@ OS System interfaces
do_bye:
        pop r0      @ reinstate this after testing: mov r0,#0
        syscall 1

@ Built-in primitive code
do_variable:
        add r1, r10, #CELL_WIDTH        @ Address of cell after CF to r1
        push r1                         @ Save it to stack
        next

do_constant:
        add r1, r10, #CELL_WIDTH        @ Address of cell after CF to r1
        ldr r1, [r1]                    @ Value to r1
        push r1
        next

do_star:
        pop r1
        pop r2
        mul r3, r1, r2
        push r3
        next

do_dup:
        pop r1          @ b, in (a b -)
        push r1
        push r1
        next

go:
        ldr r13, addr_stack_high
        ldr r11, addr_rstack_high
        @ for testing only
        ldr r9, =start
        mov r1, #5
        push r1
        @ Should end up with 25 on stack

        @ Let's do this thing!
        b do_next
        

        

@ Necessary lookup tables
addr_stack_high: .word stack_high
addr_rstack_high: .word rstack_high
start: .word cf_square, cf_bye

@        end
