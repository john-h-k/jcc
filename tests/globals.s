.data

b:
        .byte 0, 0, 0, 0



.text

.globl main
.align 4

main:
.L_main_BB_0:
        lui a0, %hi(b+0)
        addi a0, a0, %lo(b+0)
        lw a0, 0(a0)
        addi a1, zero, 0
        bne a0, a1, .L_main_BB_2
        jal zero, .L_main_BB_1
.L_main_BB_1:
        addi a0, zero, 0
        jalr zero, ra, 0
.L_main_BB_2:
        addi a0, zero, 2
        jalr zero, ra, 0

