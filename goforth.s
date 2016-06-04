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
.macro  dict label, flags, len, name, link
dict_\label:
  .hword \flags
  .hword \len
  .ascii "\name"
  .iflt \len-8
   .rept (8-\len)
    .byte 0
   .endr
  .endif
  .word dict_\link
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
dict_0: .word 0         @ Terminate dictionary search here
        code star, 0, 1, *, 0
        code plus, 0, 1, +, star
        code dup, 0, 3, dup, plus
        code nip, 0, 3, nip, dup
        code 2dup, 0, 4, 2dup, nip
        code drop, 0, 4, drop, 2dup
        code 2drop, 0, 5, 2drop, drop
        code over, 0, 4, over, 2drop
        code 2over, 0, 5, 2over, over
        code rot, 0, 3, rot, 2over
        code 2rot, 0, 4, 2rot, rot
        code swap, 0, 4, swap, 2rot
        code 2swap, 0, 5, 2swap, swap
        code qdup, 0, 4, ?dup, 2swap
        code depth, 0, 5, depth, qdup
        code pick, 0, 4, pick, depth
        code roll, 0, 4, roll, pick
        code tuck, 0, 4, tuck, roll
        code exit, 0, 4, exit, tuck
        code lit, 0, 7, literal, exit
        @ Problem here--@ is a comment marker
        @ code fetch, 0, 1, @, lit
dict_fetch:
        .hword 0
        .hword 1
        .ascii "@"
        .rept 7
        .byte 0
        .endr
        .word dict_lit
cf_fetch:
        .word do_fetch        
        @ code 2fetch, 0, 2, 2@, fetch
dict_2fetch:
        .hword 0
        .hword 2
        .ascii "2@"
        .rept 6
        .byte 0
        .endr
        .word dict_fetch
cf_2fetch:
        .word do_2fetch        
        @ code cfetch, 0, 2, c@, 2fetch
dict_cfetch:
        .hword 0
        .hword 2
        .ascii "c@"
        .rept 6
        .byte 0
        .endr
        .word dict_2fetch
cf_cfetch:
        .word do_cfetch        
        @ code rfetch, 0, 2, r@, cfetch
dict_rfetch:
        .hword 0
        .hword 2
        .ascii "r@"
        .rept 6
        .byte 0
        .endr
        .word dict_cfetch
cf_rfetch:
        .word do_rfetch        
        @ code 2rfetch, 0, 3, 2r@, rfetch
dict_2rfetch:
        .hword 0
        .hword 3
        .ascii "2r@"
        .rept 5
        .byte 0
        .endr
        .word dict_rfetch
cf_2rfetch:
        .word do_2rfetch        
        code store, 0, 1, !, 2rfetch
        code 2store, 0, 2, 2!, store
        code cstore, 0, 2, c!, 2store
        word pstore, 0, 2, +!,cstore
          .word cf_dup, cf_fetch, cf_rot, cf_plus, cf_swap, cf_store, cf_exit
        word cpstore, 0, 3, c+!,pstore
          .word cf_dup, cf_cfetch, cf_rot, cf_plus, cf_swap, cf_cstore, cf_exit
        code 2tor, 0, 3, 2>r, cpstore
        code 2rfrom, 0, 3, 2r>, 2tor
        code tor, 0, 2, >r, 2rfrom
        code rfrom, 0, 2, r>, tor
        code emit, 0, 4, emit, rfrom
        code starslash, 0, 2, */, emit
        variable here, 0, 4, here, starslash
        variable base, 0, 4, base, here
        word decimal, 0, 7, decimal, base
          .word cf_lit, 10, cf_base, cf_store, cf_exit
        word hex, 0, 3, hex, decimal
          .word cf_lit, 16, cf_base, cf_store, cf_exit
        variable blk, 0, 3, blk, decimal
        variable src, 0, 3, src, blk
        word source_id, 0, 8, source-i, src
          .word cf_src, cf_fetch, cf_exit
        word cold, 0, 4, cold, blk
          .word cf_lit, dict_end, cf_here, cf_store
          .word cf_lit, 10, cf_base, cf_store
          .word cf_lit, 0, cf_blk, cf_store
          .word cf_lit, 0, cf_src, cf_store
          .word cf_exit
        code bye, 0, 3, bye, cold

@ Save space for dictionary
dict_end:
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
        mov r0, #1                      @ For stdout
        mov r1, sp                      @ r1 points at character to write
        mov r2, #1
        syscall 4
        pop r1                          @ pop the character off the stack
        next

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
        teq r1, #0
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
 D = r6|r3 denominator (2 registers; left register initially 0)
 N = r5|r4 numerator (2 registers)
 Q = r2    Quotient
 R = r4    Remainder
 S = r0    Shift counter
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
 R = N
*/
@ Multiply/Divide, with long internal product ( n1 n2 n3 -- n1*n2/n3 )
@ TODO: Make this generally useful (/, /mod, etc.)
do_starslash:
        pop r3
        pop r2
        pop r1
        umull r4, r5, r2, r1    @ (n1*n2 -> r5|r4 (r4 - low order)        
        @ See algorithm above; N = r5|r4
        @                      D = r6|r3
        @ Leave Q in r2, R in r4
        @ Fix up later for signed operations
        mov r2, #0              @ Q
        mov r6, #0              @ Upper word of D
        mov r0, #0              @ S
.l1:    cmp r6, r5              @ while D <= N; compare upper words 
        bhi .l1x                @ if UH(D) > UH(N) exit loop
        blo .l1b                @ if UH(D) < UH(N) skip comparing LH
        cmp r3, r4              @ if UH(D) = UH(N) compare LH
        bhi .l1x                @ if UH(D) > LH(N) exit loop
.l1b:   lsls r3, r3, #1         @ Shift lower D, saving carry
        lsl  r6, r6, #1         @ Shift upper D, don't change carry
        bcc .l1c                @ Carry not set, don't add on
        add  r6, r6, #1         @ Add 1 if carry was set 
.l1c:   add  r0, r0, #1         @ Increment S
        b   .l1
.l1x:                           @ End of first loop
        lsrs r6, r6, #1         @ Shift right one bit; save Carry
        lsr  r3, r3, #1         @ Shift right
        bcc  .l2
        orr  r3, r3, #0x80000000
.l2:    
.l2a:   cmp  r0, #0
        beq  .l2x

        @ See algorithm above; N = r5|r4
        @                      D = r6|r3

        mov r1, #0              @ Initialize shift bit to 0
        cmp r6, r5              @ D > N?
        bhi .l2_endif           @ if UH(D) > UH(N), skip
        blo .l2_then            @ if UH(D) < UH(N), then D < N
        cmp r3, r4              @ We are here if the upper halves match
        bhi .l2_endif           @ same for LH(D) > UH(N)
.l2_then:
        mov r1, #1              @ Will shift a 1
        subs r4, r4, r3         @ N <- N - D
        subs r5, r5, r6
.l2_endif:
        lsl r2, r2, #1          @ Yes, Shift in the computed bit 
        add r2, r2, r1
        lsrs r6, r6, #1         @ Right shift D 1 bit
        lsr  r3, r3, #1
        bcc  .l2c
        orr  r3, r3, #0x80000000
.l2c:   sub  r0, r0, #1
        b .l2a
.l2x:
        push r2                 @ Quotient to Stack 
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
start:            .word cf_cold 
                  .word cf_lit, 25, cf_lit, 12, cf_lit, 3
                  .word cf_starslash, cf_bye

@        end
