@ GoForth source code
@    (c)  Daryl Lee, 2016
@    (cf) http://www.bradrodriguez.com/papers/moving1.htm
@         Referred to herein as "Moving Forth"
@    (cf) http://www.sifflez.org/lectures/ASE/C3.pdf
@    (cf) https://github.com/AlexandreAbreu/jonesforth
@         Referred to herein as "JonesForth"
@
@ Environment Controls
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
.macro  pushrs reg
        stmfd r11!, {\reg}
.endm
.macro  poprs reg
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
.macro  dict label, flags, len, name
dict_\label:
  .hword \flags
  .hword \len
  .ascii "\name"
  .iflt \len-8
   .rept (8-\len)
    .byte 0
   .endr
  .endif
  .word dict_latest
dict_latest=dict_\label
.endm

@ Dictionary entry for primitives (implmemented in assembler)
.macro code label, flags, len, name
  dict \label, \flags, \len, \name
  cf_\label: .word do_\label
.endm

@ Dictionary entry for high-level words (implemented in Forth)
.macro word label, flags, len, name
  dict \label, \flags, \len, \name
  cf_\label: .word do_enter
.endm

@ Dictionary entry for built-in Forth VARIABLEs
.macro variable label, flags, len, name
  dict \label, \flags, \len, \name
  cf_\label: .word do_variable
  .word 0
.endm

@ Dictionary entry for built-in Forth CONSTANTs
.macro constant label, flags, len, name, value
  dict \label, \flags, \len, \name
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
dict_0: .word 0         @ Terminate dictionary search here

        @ 6.1.0010 !
        code store, 0, 1, !

        @ 6.1.0090 *
        code star, 0, 1, *

        @ 6.1.0100 */
        word starslash, 0, 2, */
          .word cf_starslashmod, cf_nip, cf_exit

        @ 6.1.0110 */
        code starslashmod, 0, 5, */mod

        @ 6.1.0120 +
        code plus, 0, 1, +

        @ 6.1.0130 +!
        word pstore, 0, 2, +!
          .word cf_dup, cf_fetch, cf_rot, cf_plus, cf_swap, cf_store, cf_exit

        @ Problem here--@ is a comment marker; , is a parameter separator
        @ Similar problem elsewhere

        @ 6.1.0150 ,
        @ code comma, 0, 1, ','
dict_comma:
        .hword 0
        .hword 1
        .ascii ","
        .rept 7
        .byte 0
        .endr
        .word dict_latest
cf_comma:
        .word do_comma
dict_latest=dict_comma

        @ 6.1.0160 -
        code minus, 0, 1, -

        @ 6.1.0230 /
        word slash, 0 1, /
          .word cf_slashmod, cf_nip, cf_exit

        @ 6.1.0240 /MOD
        word slashmod, 0 4, /mod
          .word cf_lit, 1, cf_rot, cf_rot, cf_starslashmod, cf_exit

        @ 6.1.0250 0<
        code 0less, 0, 2, 0<
        
        @ 6.1.0270 0=
        @code 0equal, 0, 2, 0=
dict_0equal:
        .hword 0
        .hword 2
        .ascii "0="
        .rept 6
        .byte 0
        .endr
        .word dict_latest
cf_0equal:
        .word do_0equal
dict_latest=dict_0equal
       
        @ 6.1.0290 1+
        word oneplus, 0, 2, 1+
          .word cf_lit, 1, cf_plus, cf_exit 
       
        @ 6.1.0300 10
        word oneminus, 0, 2, 1-
          .word cf_lit, 1, cf_minus, cf_exit 

        @ 6.1.0310 2!
        code 2store, 0, 2, 2!

        @ 6.1.0320 2*
        word 2star, 0, 2, 2*
          .word cf_lit, 2, cf_star, cf_exit

        @ 6.1.0330 2/
        word 2slash, 0, 2, 2/
          .word cf_lit, 2, cf_slash, cf_exit

        @ 6.1.0350 2@
        @ code 2fetch, 0, 2, 2@
dict_2fetch:
        .hword 0
        .hword 2
        .ascii "2@"
        .rept 6
        .byte 0
        .endr
        .word dict_latest
cf_2fetch:
        .word do_2fetch        
dict_latest=dict_2fetch

        @ 6.1.0370 2DROP
        code 2drop, 0, 5, 2drop

        @ 6.1.0380 2DUP
        code 2dup, 0, 4, 2dup

        @ 6.1.0400 2OVER
        code 2over, 0, 5, 2over

        @ 6.1.0430 2SWAP
        code 2swap, 0, 5, 2swap

        @ 6.1.0480 <
        code less, 0, 1, <

        @ 6.1.0530 =
        @code equal, 0, 1, =
dict_equal:
        .hword 0
        .hword 1
        .ascii "="
        .rept 7
        .byte 0
        .endr
        .word dict_latest
cf_equal:
        .word do_equal
dict_latest=dict_equal

        @ 6.1.0540 >
        code greater, 0, 1, >

        @ 6.1.0580 >R
        code tor, 0, 2, >r

        @ 6.1.0630 ?DUP
        code qdup, 0, 4, ?dup

        @ 6.1.0650 @
        @ code fetch, 0, 1, @
dict_fetch:
        .hword 0
        .hword 1
        .ascii "@"
        .rept 7
        .byte 0
        .endr
        .word dict_latest
cf_fetch:
        .word do_fetch        
dict_latest=dict_fetch

        @ 6.1.0705 ALIGN
        word align, 0, 5, align
          .word cf_lit, cf_here, cf_dup, cf_fetch, cf_aligned, cf_store
          .word cf_exit

        @ 6.1.0706 ALIGNED
        code aligned, 0, 7, aligned

        @ 6.1.0710 ALLOT
        code allot, 0, 5, allot

        @ 6.1.0720 AND
        code and, 0, 3, and

        @ 6.1.0750 BASE
        variable base, 0, 4, base

        @ 6.1.0770 BL
        constant bl, 0, 2, bl, 0x20

        @ 6.1.0850 C!
        code cstore, 0, 2, c!

        @ 6.1.0860 C,
        @ code ccomma, 0, 2, 'c,'
dict_ccomma:
        .hword 0
        .hword 2
        .ascii "c,"
        .rept 6
        .byte 0
        .endr
        .word dict_latest
cf_ccomma:
        .word do_ccomma
dict_latest=dict_ccomma

        @ 6.1.0870 C@
        @ code cfetch, 0, 2, c@
dict_cfetch:
        .hword 0
        .hword 2
        .ascii "c@"
        .rept 6
        .byte 0
        .endr
        .word dict_latest
cf_cfetch:
        .word do_cfetch        
dict_latest=dict_cfetch

        @ 6.1.0880 CELL+ 
        word cellplus, 0, 5, cell+
          .word cf_lit, CELL_WIDTH, cf_plus, cf_exit

        @ 6.1.0890 CELLS
        code cells, 0, 5, cells

        @ 6.1.0897 CHAR+
        word charplus, 0, 5, char+
          .word cf_lit, 1, cf_plus, cf_exit

        @ 6.1.0898 CHARS
        word chars, 0, 5, chars
          .word cf_exit     @ In this implementation, it's a no-op

        @ 6.1.0980 COUNT
        code count, 0, 5, count
        
        @ 6.1.0990 CR
        word cr, 0, 2, cr
          .word cf_lit, 10, cf_emit, cf_exit
  
        @ 6.1.1170 DECIMAL
        word decimal, 0, 7, decimal
          .word cf_lit, 10, cf_base, cf_store, cf_exit

        @ 6.1.1200 DEPTH
        code depth, 0, 5, depth

        @ 6.1.1260 DROP
        code drop, 0, 4, drop

        @ 6.1.1290 DUP
        code dup, 0, 3, dup

        @ 6.1.1320 EMIT
        code emit, 0, 4, emit

        @ 6.1.1380 EXIT 
        code exit, 0, 4, exit

        @ 6.1.1550 FIND
        code find, 0, 4, find
        
        @ 6.1.1650 HERE
        variable here, 0, 4, here

        @ 6.1.1720 INVERT
        code invert, 0, 6, invert
        
        @ 6.1.1780 LITERAL
        code lit, 0, 7, literal

        @ 6.1.1805 LSHIFT
        code lshift, 0, 6, lshift

        @ 6.1.1870 MAX
        code max, 0, 3, max

        @ 6.1.1880 MIN
        code min, 0, 3, min

        @ 6.1.1910 NEGATE
        word negate, 0, 7, negate
          .word cf_lit, 0, cf_swap, cf_minus, cf_exit

        @ 6.1.1980 OR
        code or, 0, 2, or

        @ 6.1.1990 OVER
        code over, 0, 4, over

        @ 6.1.2060 R>
        code rfrom, 0, 2, r>

        @ 6.1.2070 R@
        @ code rfetch, 0, 2, r@
dict_rfetch:
        .hword 0
        .hword 2
        .ascii "r@"
        .rept 6
        .byte 0
        .endr
        .word dict_latest
cf_rfetch:
        .word do_rfetch        
dict_latest=dict_rfetch

        @ 6.1.2160 ROT
        code rot, 0, 3, rot

        @ 6.1.2162 RSHIFT
        code rshift, 0, 6, rshift

        @ 6.1.2220 SPACE
        word space, 0, 5, space
          .word cf_bl, cf_emit, cf_exit

        @ 6.1.2230 SPACES
        word spaces, 0, 6, spaces
spaces_1: .word cf_qdup, cf_0equal, cf_invert, cf_0branch, spaces_2-.
          .word cf_space, cf_oneminus, cf_branch, spaces_1-.
spaces_2: .word cf_exit

        @ 6.1.2260 SWAP
        code swap, 0, 4, swap

        @ 6.1.2310 TYPE
        code type, 0, 4, type

        @ 6.1.2490 XOR
        code xor, 0, 3, xor

        @ 6.2.0260 0<>
        word not0, 0, 3, 0<>
          .word cf_0equal, cf_invert, cf_exit

        @ 6.2.0280 0>
        code 0greater, 0, 2, 0>

        @ 6.2.0340 2>R
        code 2tor, 0, 3, 2>r

        @ 6.2.0410 2R>
        code 2rfrom, 0, 3, 2r>

        @ 6.2.0415 2R@
        @ code 2rfetch, 0, 3, 2r@
dict_2rfetch:
        .hword 0
        .hword 3
        .ascii "2r@"
        .rept 5
        .byte 0
        .endr
        .word dict_latest
cf_2rfetch:
        .word do_2rfetch        
dict_latest=dict_2rfetch

        @ 6.2.0500 <>
        word notequal, 0, 2, <>
          .word cf_0equal, cf_invert, cf_exit

        @ 6.2.1485 FALSE
        word false, 0, 5, false
          .word cf_lit, 0, cf_exit

        @ 6.2.1660 HEX
        word hex, 0, 3, hex
          .word cf_lit, 16, cf_base, cf_store, cf_exit

        @ 6.2.1930 NIP
        code nip, 0, 3, nip

        @ 6.2.2030 PICK
        code pick, 0, 4, pick

        @ 6.2.2150 ROLL
        code roll, 0, 4, roll

        @ 6.2.2218 SOURCE-ID
        word source_id, 0, 8, source-i
          .word cf_src, cf_fetch, cf_exit

        @ 6.2.2298 TRUE
        word true, 0, 4, true
          .word cf_false, cf_invert, cf_exit

        @ 6.2.2300 TUCK
        code tuck, 0, 4, tuck

        @ 7.6.1.0790 BLK
        variable blk, 0, 3, blk

        @ 8.6.2.0420 2ROT
        code 2rot, 0, 4, 2rot

        @ C+! is not in the standarc
        word cpstore, 0, 3, c+!
          .word cf_dup, cf_cfetch, cf_rot, cf_plus, cf_swap, cf_cstore, cf_exit

        @ BRANCH and 0BRANCH are used internally by IF, BEGIN, etc.
        code branch, 0, 6, branch
        code 0branch, 0, 7, 0branch


        @ SRC is an internal variable used by SOURCE-ID
        variable src, 0, 3, src

        @ Interpreter words
        word cold, IMMEDIATE, 4, cold
          .word cf_lit, dict_end, cf_here, cf_store
          .word cf_lit, 10, cf_base, cf_store
          .word cf_lit, 0, cf_blk, cf_store
          .word cf_lit, 0, cf_src, cf_store
          .word cf_exit
        code bye, 0, 3, bye       @@@ Keep as last word

@ Save space for dictionary
dict_end: .word dict_bye
        .rept 16000
        .word 0
        .endr

@ testing area; see also start:

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
        
@ The Forth Interpreter

do_enter:                               @ Pseudocode from MF:
        pushrs r9                      @ PUSH IP
        add r9, r10, #CELL_WIDTH        @ W+2 -> IP
        b do_next
do_exit:
        poprs r9                       @ POP IP (from return stack)
do_next:                                @ JUMP to interpreter ("NEXT")
        ldr r10, [r9]                   @ (IP) -> W
        add r9, r9, #CELL_WIDTH         @ IP + 2 -> IP
        ldr r12, [r10]                  @ (W) -> X
        mov pc,r12                      @ JP (X)

@ OS System interfaces

@ Call sys_exit, with exit code TOS
do_bye:
        pop r0      
        syscall 1

@ Write TOS to console (ref: man 2 write)
do_emit:
        mov r2, #1                      @ Send one character
        mov r1, sp                      @ r1 points at character to write
do_emit2:
        mov r0, #1                      @ For stdout
        syscall 4
        pop r1                          @ pop the character off the stack
        next

@ Write text to console (ref: man 2 write)
do_type:
        pop r2                          @ Character count
        pop r1                          @ Address of text
        b do_emit2

@ Built-in primitive code
@ Push address of defined variable
do_variable:
        add r1, r10, #CELL_WIDTH        @ Address of cell after CF to r1
        push r1                         @ Save it to stack
        next

@ Push current value pointed to by TOS (see 'variable')
do_fetch:
        pop r1
        ldr r1,[r1]
        push r1
        next

@ Get TORS to stack
do_rfetch:
        poprs r1
        pushrs r1
        push r1
        next

@ Get two values from TORS to stack (R: n1 n2 -- n1 n2) (S: -- n1 n2 )
do_2rfetch:
        poprs r2
        poprs r1
        pushrs r1
        pushrs r2
        push r1
        push r2
        next

@ Push low-order byte of current value pointed to by TOS (see 'variable')
do_cfetch:
        pop r1
        ldrb r1,[r1]
        push r1
        next

@ Push current value pair pointed to by TOS (see 'variable') (see do_2store)
do_2fetch:
        pop r1
        ldr r2,[r1]
        ldr r3,[r1, #CELL_WIDTH]
        push r3
        push r2
        next

@ Move TOS to RS  ( n -- ) ( -- n )
do_tor:
        pop r1
        pushrs r1
        next

@ Push two values on stack to RS ( n1 n2 -- ) (R: -- n1 n2 )
do_2tor:
        pop r2
        pop r1
        pushrs r1
        pushrs r2
        next

@ Pop TORS to stack   (R: n -- ) ( -- n)
do_rfrom:
        poprs r1
        push r1
        next

@ Pop two TORS to stack (R: n1 n2 -- ) ( -- n1 n2)
do_2rfrom:
        poprs r2
        poprs r1
        push r1
        push r2
        next

@ Store second-on-stack at location specified by TOS
do_store:
        pop r1                          @ Address
        pop r2                          @ Value
        str r2, [r1]
        next

@ Store low-order byt second-on-stack at location specified by TOS
do_cstore:
        pop r1                          @ Address
        pop r2                          @ Value
        strb r2, [r1]
        next

@ Store second-on-stack  pair at location specified by TOS
do_2store:
        pop r1                          @ Address
        pop r2                          @ Value
        pop r3
        str r2, [r1]                    @ TODO: Are these in the right order?
        str r3, [r1, #CELL_WIDTH]
        next

@ Push value of defined constant
do_constant:
        add r1, r10, #CELL_WIDTH        @ Address of cell after CF to r1
        ldr r1, [r1]                    @ Value to r1
        push r1
        next

@ Push value of next word in dictionary
do_lit:
        ldr r1, [r9]
        add r9, r9, #CELL_WIDTH
        push r1
        next

@ Multiply two cells TOS, leaving one-cell product TOS ( n1 n2 -- n1*n2 )
do_star:
        pop r1
        pop r2
        mul r3, r1, r2
        push r3
        next

@ Add two cells TOS, leaving one-cell sum TOS ( n1 n2 -- n1+n2 )
do_plus:
        pop r1
        pop r2
        add r3, r1, r2
        push r3
        next

@ Subtract TOS from 2d, leaving one-cell differenct TOS ( n1 n2 -- n1-n2 )
do_minus:
        pop r2
        pop r1
        sub r3, r1, r2
        push r3
        next

@ Convert counted string to count and c-addr
do_count:               @ ( c-addr -- c-addr u )
        pop r0
        ldrb r1, [r0]   @ Get count
        add r0, r0, #1  @ Increment address
        push r0
        push r1
        next

@ Numerical comparisons
do_0less:
        pop r0          @ Get value to test
        mov r1, #0      @ Init flag to 0 (false)
        cmp r0, #0
        bge do_0less1   @ Skip flag change if >= 0
        sub r1, r1, #1  @ Make flag -1 (true)
do_0less1:    push r1
        next

do_0equal:
        pop r0          @ Get value to test
        mov r1, #0      @ Init flag to 0 (false)
        cmp r0, #0
        bne do_0e1      @ Skip flag change if != 0
        sub r1, r1, #1  @ Make flag -1 (true)
do_0e1: push r1
        next

do_0greater:
        pop r0          @ Get value to test
        mov r1, #0      @ Init flag to 0 (false)
        cmp r0, #0
        ble do_0g1      @ Skip flag change if != 0
        sub r1, r1, #1  @ Make flag -1 (true)
do_0g1: push r1
        next

do_equal:               @ ( n1 n2 -- flag true iff n1 = n2 )
        pop r2          @ Get values
        pop a1
        mov r0, #0      @ Initialize flag to false
        cmp r1, r2
        bne do_equl1    @ Skip change if r1 != r2
        sub r0, #1      @ Change flag to true
do_equl1:
        push r0
        next

do_less:                @ ( n1 n2 -- flag true iff n1 < n2 )
        pop r2          @ Get values
        pop r1
        mov r0, #0      @ Initialize flag to false
        cmp r1, r2
        bge do_less1    @ Skip change if r1 >= r2
        sub r0, #1      @ Change flag to true
do_less1:
        push r0
        next

do_greater:             @ ( n1 n2 -- flag true iff n1 > n2 )
        pop r2          @ Get values
        pop r1
        mov r0, #0      @ Initialize flag to false
        cmp r1, r2
        ble do_grtr1    @ Skip change if r1 <= r2
        sub r0, #1      @ Change flag to true
do_grtr1:
        push r0
        next

@ Bitwise logical operations
do_and:                 @ ( n1 n2 -- n1 & n2 )
        pop r1
        pop r2
        and r0, r1, r2
        push r0
        next

do_or:                 @ ( n1 n2 -- n1 | n2 )
        pop r1
        pop r2
        orr r0, r1, r2
        push r0
        next

do_xor:                @ ( n1 n2 -- n1 ^ n2 )
        pop r1
        pop r2
        eor r0, r1, r2
        push r0
        next

do_invert:              @ ( n -- n' )
        pop r1
        mvns r1, r1     @ Invert the bits
        push r1
        next

do_lshift:              @ ( n u -- n left-shifted u bits )
        pop r2
        pop r1
        lsl r1, r2
        push r1
        next

do_rshift:              @ ( u1 u2 -- u1 right-shifted u2 bits )
        pop r2
        pop r1
        lsr r1, r2
        push r1
        next

do_max:                 @ ( n1 n2 -- n3, where n3 is max of n1, n2 )
        pop r2
        pop r1
        cmp r1, r2
        bge do_max1
        mov r1, r2
do_max1:
        push r1
        next

do_min:                 @ ( n1 n2 -- n3, where n3 is min of n1, n2 )
        pop r2
        pop r1
        cmp r1, r2
        ble do_min1
        mov r1, r2
do_min1:
        push r1
        next

@ Duplicate TOS   ( n1 -- n1 n1 )
do_dup:
        pop r1
        push r1
        push r1
        next

@ TODO: A faster way?
@ Duplicate two TOS ( n1 n2 -- n1 n2 n1 n2)
do_2dup:
        pop r2
        pop r1
        push r1
        push r2
        push r1
        push r2
        next

@ Drop TOS    ( n1 -- )
do_drop:
        pop r1
        next

@ Drop 2d on stack    ( n1 n2 -- n2 )
do_nip:
        pop r2
        pop r1
        push r2
        next

@ Duplicate the nth item on stack ( n -- n )
do_pick:
        pop r1          @ Get stack index
        lsl r1, #2      @ multiply by 4
        add r2, r13, r1 @ Get address of indexed value
        ldr r1,[r2]
        push r1
        next

@ Roll the nth item to TOS.  E.g.: ( n1 n2 n3 n4 2 -- n1 n3 n4 n2 )
do_roll:
        pop r1                  @ get count
        mov r3, r1
        lsl r3, #2
        mov r2,sp               @ get current sp
        add r3, r2, r3          @ make r3 point at nth item
        ldr r4,[r3]             @ and get a copy of it.
.l:     ldr r5,[r3, #-4]        @ get item just above current target
        str r5,[r3]             @ and copy it
        sub r3, r3, #4          @ move pointer
        sub r1, #1              @ decrement counter
        teq r1, #0
        bne .l                  @ and loop
        str r4,[r3]             @ put rolled item on top
        next

@ Tuck the TOS under 2d on stack: ( n1 n2 -- n2 n1 n2 )
do_tuck:
        pop r2
        pop r1
        push r2
        push r1
        push r2
        next

        
@ Drop two TOS  ( n1 n2 -- )
do_2drop:    
        pop r1
        pop r1
        next

@ TODO: A faster way?
@ Copy 2d on stack to TOS ( n1 n2 -- n1 n2 n1 )
do_over:
        pop r2
        pop r1
        push r1
        push r2
        push r1
        next

@ TODO: A faster way?
@ Copy 2d pair on stack to TOS ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
do_2over:
        pop r4
        pop r3
        pop r1
        pop r1
        push r1
        push r2
        push r3
        push r4
        push r1
        push r2
        next

@ TODO: A faster way?
@ Bring 3d on stack to TOS ( n1 n2 n3 -- n2 n3 n1 )
do_rot:
        pop r3
        pop r2
        pop r1
        push r2
        push r3
        push r1
        next

@ TODO: A faster way?
@ Bring 3d pair on stack to TOS ( n1 n2 n3 n4 n5 n6 -- n3 n4 n5 n6 n1 n2 )
do_2rot:
        pop r6
        pop r5
        pop r4
        pop r3
        pop r2
        pop r1
        push r3
        push r4
        push r5
        push r6
        push r1
        push r2
        next

@ Swap the two items TOS  ( n1 n2 -- n2 n1 )
do_swap:
        pop r2
        pop r1
        push r2
        push r1
        next

@ Swap the two pairs TOS  ( n1 n2 n3 n4-- n3 n4 n1 n2 )
do_2swap:
        pop r4
        pop r3
        pop r2
        pop r1
        push r3
        push r4
        push r2
        push r2
        next

@ Dup TOS if non-zero   ( n -- 0 | n n )
do_qdup:
        pop r1
        cmp r1, #0
        beq .s
        push r1
.s:     push r1
        next

@ How many item were on the stack before this word ran? ( -- )
do_depth:
        ldr r1, addr_stack_high
        sub r1, r1, r13         @ Number of bytes on stack
        lsr r1, #2              @ Divide by 4
        push r1
        next

/* 
 Algorithm for dividing a 64-bit number by a 32-bit number
 D = r6|r0 denominator (2 registers; left register initially 0)
 N = r1|r2 numerator (2 registers)
 Q = r5    Quotient
 S = r3    Shift counter
 All shifting, comparing, and subtracting are 2-register operations
 
 Q = S = 0;
 while  D <= N:
   shift left D one bit
   increment S
 shift right D one bit
 while S > 0:
   set shift bit to 0
   if D <= N:
     set shift bit to 1
     N <-- N - D
   shift computed bit left into Q
   shift right D one bit
   S--
 Q holds quotient
 N(low) holds remainder


 For Floored and Symmetric Division, see Forth Programmer's Handbook, 2.2.1
   Let Sn = Sign of Numerator
       Sd = Sign of Denomerator
       Qn = Quotient from division of absolute values
       Rn = Remaninder from division of absolute values
       Qf = Quotient of Floored division
       Rf = Remainder of Floored division
       Qs = Quotient of Symmetric division
       Rs = Remainder of Symmetric division
 Floored division:
   Sn = Sd -> Qf = Qn, Rf = Rn*Sd
        else  Rn != 0 -> Qf = -Qn - 1, Rf = Den - Rn*Sd
                    else Qf = -Qn, Rf = Rn
 Symmetric division:
   Qs = Qs*Sd*Sn, Rs = Rn*Sd
*/
div_64_32:
        @ In keeping with the recommendations for calling subroutines:
        @  Inputs
        @   r0    = Denominator
        @   r1|r2 = Numerator
        @  Outputs
        @   r0 = Quotient
        @   r1 = Remainder
        @  All other registers preserved

        @ See algorithm above; N = r1|r2
        @                      D = r6|r0
        @ Leave Q in r0, R in r1
        @ Fix up later for signed operations
        stmfd r13!, {r3-r6}
        mov r5, #0              @ Q
        mov r6, #0              @ Upper word of D
        mov r3, #0              @ S
.l1:    cmp r6, r1              @ while D <= N; compare upper words 
        bhi .l1x                @ if UH(D) > UH(N) exit loop
        blo .l1b                @ if UH(D) < UH(N) skip comparing LH
        cmp r0, r2              @ if UH(D) = UH(N) compare LH
        bhi .l1x                @ if UH(D) > LH(N) exit loop
.l1b:   lsls r0, r0, #1         @ Shift lower D, saving carry
        lsl  r6, r6, #1         @ Shift upper D, don't change carry
        bcc .l1c                @ Carry not set, don't add on
        add  r6, r6, #1         @ Add 1 if carry was set 
.l1c:   add  r3, r3, #1         @ Increment S
        b   .l1
.l1x:                           @ End of first loop
        lsrs r6, r6, #1         @ Shift right one bit; save Carry
        lsr  r0, r0, #1         @ Shift right
        bcc  .l2
        orr  r0, r0, #0x80000000
.l2:    
.l2a:   cmp  r3, #0
        beq  .l2x
        mov r4, #0              @ Initialize shift bit to 0
        cmp r6, r1              @ D > N?
        bhi .l2_endif           @ if UH(D) > UH(N), skip
        blo .l2_then            @ if UH(D) < UH(N), then D < N
        cmp r0, r2              @ We are here if the upper halves match
        bhi .l2_endif           @ same for LH(D) > UH(N)
.l2_then:
        mov r4, #1              @ Will shift a 1
        subs r2, r2, r0         @ N <- N - D
        subs r1, r1, r6
.l2_endif:
        lsl r5, r5, #1          @ Yes, Shift in the computed bit 
        add r5, r5, r4
        lsrs r6, r6, #1         @ Right shift D 1 bit
        lsr  r0, r0, #1
        bcc  .l2c
        orr  r0, r0, #0x80000000
.l2c:   sub  r3, r3, #1
        b .l2a
.l2x:
        mov r0, r5              @ Quotient to r0
        mov r1, r2              @ Remainder to r1
        ldmfd r13!, {r3-r6}             @ Restore registers
        bx lr

@ Multiply/Divide, with long internal product ( n1 n2 n3 -- n1*n2/n3 )
@ TODO: Make this generally useful (/, /mod, etc.)
do_starslashmod:
        pop r0                  @ n3
        pop r2                  @ n2
        pop r1                  @ n1
        smull r4, r3, r2, r1    @ (n1*n2 -> r1|r2 (r2 - low order)        
        mov r2, r4
        mov r1, r3
        @ Save signs and send absolute values to subroutine
        mov r3, #1              @ Numerator's sign in r3 (1 or -1)
        cmp r1, #0              @ The sign of r1|r2 is in r1
        bge .dosl1
        mov r3, #-1
        mvn r1, r1              @ 64-bit absolute value
        mvn r2, r2
        adds r2, r2, #1
        adc r1, r1, #0
.dosl1: mov r4, #1              @ Denominator's sign in r4 (1 or -1)
        cmp r0, #0
        bge .dosl2
        mov r4, #-1
        mvn r0, r0
        add r0, r0, #1
.dosl2:
        @ Now call the division subroutine
        @ But save the denominator first
        push r0
        bl div_64_32
        pop  r5                 @ Original Denominator to r5
.dosl3:
        @ This function obeys Floored division
        cmp r3, r4              @ Sd = Sn?
        bne .dosl4
        mul r2, r1, r4          @ Yes; Qf = Qn; Rf = Rn * Sd
        mov r1, r2
        b .dosl_done
.dosl4: cmp r1, #0              @ Rn = 0?
        bne .dosl5
        mvn r0, r0              @ Yes; Qf = -Qn - 1...
        add r0, r0, #1
        sub r0, r0, #1
        mul r2, r1, r4          @ ... Rf = Den - Rn*Sd
        sub r1, r5, r2
        b .dosl_done
.dosl5: mvn r0, r0              @ No; Qf = -Qn; Rf = Rn
        add r0, r0, #1
.dosl_done:
        push r1                 @ Remainder to stack
        push r0                 @ Quotient to stack
        next

@ Branching functions
do_branch:                      @ Branch always; offset is pointed to by IP
        ldr r0, [r9]            @ Get the offset
        add r9, r9, r0          @ Adjust IP by offset
        next

do_0branch:                     @ Branch if TOS = 0
        pop r1
        cmp r1, #0
        beq do_branch
        add r9, r9, #CELL_WIDTH @ Didn't branch; skip offset
        next
@ Case-insensitive word search
do_find:                        @ ( c-addr -- c-addr 0 | xt 1 | xt -1 )
        pop r8                  @ Hold onto the c-addr; we may need it later
        ldrb r1, [r8]           @ Get one-byte count
        add r0, r8, #1          @ r0 holds the target address, r1 the count
        ldr r2, addr_here       @ Get current value of HERE
        ldr r2,[r2]
do_find_loop:
        ldrh r3, [r2, #2]       @ r3 now holds length of dictionary entry
        add r4, r2, #CELL_WIDTH
                                @ Compare [r0, r1] to [r4, r3]
                                @   where [rn, rm] = [address, count]
        cmp r1, r3              @ Do lengths match?
        bne do_find_mismatch
        @ r0  : target starting address
        @ r1  : target length
        @ r2  : dictionary entry address
        @ r3  : dictionary name length
        @ r4  : dictionary name address
        @ r8  : target c-addr
        cmp r3, #8              @ Test only the first 8 characters
        ble do_find_1
        mov r3, #8
do_find_1:
        mov r5, #0              @ r5 is the index
do_find_next:
        ldrb r6, [r0, r5]       @ Get char from target
        cmp r6, #'a             @ Upcase it if alphabetic
        blt do_find_2
        cmp r6, #'z
        bgt do_find_2
        and r6, #0xdf
do_find_2:
        ldrb r7, [r4, r5]        @ Get char from dictionary
        cmp r7, #'a             @ Upcase it if alphabetic
        blt do_find_3
        cmp r7, #'z
        bgt do_find_3
        and r7, #0xdf
do_find_3:
        cmp r6, r7              @ Do characters match?
        bne do_find_mismatch
        add r5, #1              @ Yes, increment index
        cmp r5, r3              @ Tested all characters?
        bne do_find_next        @ No, keep testing
do_find_found:
        add r0, r2, #16         @ Found match!  Get xt
        push r0
        mov r1, #-1             @ Return -1 if not immediate
        ldrh r0, [r2]           @ Get immediate bit
        ands r0, #IMMEDIATE
        beq do_find_5
        mov r1, #1              @ Else return 1
do_find_5:
        push r1
        b do_find_done
do_find_mismatch:               @ Mismatch--keep searching
        ldr r2, [r2, #12]       @ Get address of preceding entry
        cmp r2, #0
        bne do_find_loop
do_find_failed:
        push r8                 @ We're here if there is no matching entry
        mov r0, #0
        push r0        
do_find_done:
        next

@ Add word to dictionary
do_comma:
        pop r0                  @ Get word from TOS
        ldr r1, addr_here       @ Get address of HERE
        ldr r2, [r1]            @ Get address of next dictionary entry
        str r0, [r2]            
        add r0, r2, #CELL_WIDTH @ Bump HERE
        str r0, [r1]
        next

@ Add byte to dictionary
do_ccomma:
        pop r0                  @ Get byte from TOS
        ldr r1, addr_here       @ Get address of HERE
        ldr r2, [r1]            @ Get address of next dictionary entry
        strb r0, [r2]            
        add r0, r2, #1          @ Bump HERE
        str r0, [r1]
        next

@ Align address (depends on 4-byte cells)
do_aligned:
        pop r0                  @ Get address from TOS
        add r0, r0, #3          @ Add 3, and mask overflow
        lsr r0, #2
        lsl r0, #2
        push r0
        next

@ Allot space in the dictionary
do_allot:
        pop r0                  @ Get byte count (positive, negative, or zero)
        ldr r1, addr_here       @ Get address of HERE
        ldr r2, [r1]            @ Get address of next dictionary entry
        add r0, r2, r0          @ Bump HERE by specified amount
        str r0, [r1]            @ Store result in HERE
        next

@ Compute the size in bytes of the specified number of cells
do_cells:
        pop r0                  @ Get number of cells
        lsl r0, #2              @ Depends on CELL_WIDTH = 4
        push r0
        next

@ Main entry point
_start:
        ldr r13, addr_stack_high
        ldr r11, addr_rstack_high
        
        @ for testing only; eventually replace with real Forth startup
        ldr r9, =start

        @ Let's do this thing!
        b do_next

@ Necessary lookup tables
addr_dict_end:    .word dict_end
addr_stack_high:  .word stack_high
addr_rstack_high: .word rstack_high
addr_here:        .word cf_here+CELL_WIDTH
text:             .ascii "Hello, world\n"
text_len=.-text
target:           .byte 4
                  .ascii "over"
start:            .word cf_cold 
                  .word cf_lit, 5, cf_0greater, cf_0branch, start1-.
                  .word cf_lit, 0x31, cf_emit, cf_branch, start2-.
start1:           .word cf_lit, 0x30, cf_emit
start2:           .word cf_lit, 0, cf_bye

@        end
