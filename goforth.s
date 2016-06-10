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
        code type, 0, 4, type, emit
        code starslash, 0, 2, */, type
        code branch, 0, 6, branch, starslash
        code 0branch, 0, 7, 0branch, branch
        code find, 0, 4, find, 0branch
        variable here, 0, 4, here, find
        variable base, 0, 4, base, here
        word decimal, 0, 7, decimal, base
          .word cf_lit, 10, cf_base, cf_store, cf_exit
        word hex, 0, 3, hex, decimal
          .word cf_lit, 16, cf_base, cf_store, cf_exit
        variable blk, 0, 3, blk, decimal
        variable src, 0, 3, src, blk
        word source_id, 0, 8, source-i, src
          .word cf_src, cf_fetch, cf_exit
        word cold, IMMEDIATE, 4, cold, blk
          .word cf_lit, dict_bye, cf_here, cf_store
          .word cf_lit, 10, cf_base, cf_store
          .word cf_lit, 0, cf_blk, cf_store
          .word cf_lit, 0, cf_src, cf_store
          .word cf_exit
        code bye, 0, 3, bye, cold       @@@ Keep as last word

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
do_starslash:
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
        ldr r2,[r2, #CELL_WIDTH]
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
addr_here:        .word cf_here
text:             .ascii "Hello, world\n"
text_len=.-text
target:           .byte 4
                  .ascii "xxxx"
start:            .word cf_cold 
@                  .word cf_lit, 0, cf_0branch, st1-.    @ 1 if
@                  .word   cf_lit, 3, cf_branch, st2-.   @   3
@st1:              .word   cf_lit, 4                     @ else 4
@st2:                                                    @ then
@                   .word cf_lit, text, cf_lit, text_len, cf_type
                   .word cf_lit, target, cf_find 
                   .word cf_bye

@        end
