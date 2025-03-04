.text
.globl mk_vec3
.align 2
.type	mk_vec3, @function

mk_vec3:
        sw ra, -4(sp)
        sw sp, -8(sp)
        addi sp, sp, -48
.BB_0:
        mv a3, a0
        mv a0, sp
        mv a1, zero
        addi a2, zero, 12
        addi t1, sp, 12
        fsw fa0, 0(t1)
        addi t1, sp, 16
        fsw fa1, 0(t1)
        addi t1, sp, 20
        fsw fa2, 0(t1)
        addi t1, sp, 24
        sw a3, 0(t1)
        15: auipc ra, %pcrel_hi(memset+0)
        jalr ra, ra, %pcrel_lo(15b)
        addi t1, sp, 24
        lw a3, 0(t1)
        addi t1, sp, 20
        flw fa2, 0(t1)
        addi t1, sp, 16
        flw fa1, 0(t1)
        addi t1, sp, 12
        flw fa0, 0(t1)
        fsw fa0, 0(sp)
        fsw fa1, 4(sp)
        fsw fa2, 8(sp)
        mv a1, sp
        addi a2, zero, 12
        mv a0, a3
        31: auipc ra, %pcrel_hi(memcpy+0)
        jalr ra, ra, %pcrel_lo(31b)
        addi sp, sp, 48
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0

.text
.globl mk_small
.align 2
.type	mk_small, @function

mk_small:
        sw ra, -4(sp)
        sw sp, -8(sp)
        addi sp, sp, -32
.BB_0:
        mv a3, a0
        mv a4, a1
        mv a0, sp
        mv a1, zero
        addi a2, zero, 8
        addi t1, sp, 8
        sw a3, 0(t1)
        addi t1, sp, 12
        sw a4, 0(t1)
        12: auipc ra, %pcrel_hi(memset+0)
        jalr ra, ra, %pcrel_lo(12b)
        addi t1, sp, 12
        lw a4, 0(t1)
        addi t1, sp, 8
        lw a3, 0(t1)
        sw a3, 0(sp)
        sw a4, 4(sp)
        lw a1, 4(sp)
        lw a0, 0(sp)
        addi sp, sp, 32
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0

.text
.globl mk_big
.align 2
.type	mk_big, @function

mk_big:
        sw ra, -4(sp)
        sw sp, -8(sp)
        addi sp, sp, -80
.BB_0:
        mv a1, zero
.BB_1:
        addi a2, zero, 16
        bge a1, a2, .BB_4
        jal zero, .BB_2
.BB_2:
        mv a2, sp
        addi a3, zero, 4
        mul a3, a1, a3
        add a2, a2, a3
        sw a1, 0(a2)
.BB_3:
        addi a2, zero, 1
        add a1, a1, a2
        jal zero, .BB_1
.BB_4:
        mv a1, sp
        addi a2, zero, 64
        17: auipc ra, %pcrel_hi(memcpy+0)
        jalr ra, ra, %pcrel_lo(17b)
        addi sp, sp, 80
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0

.text
.globl main
.align 2
.type	main, @function

main:
        sw ra, -4(sp)
        sw sp, -8(sp)
        addi sp, sp, -112
.BB_0:
        addi a0, zero, 1
        fcvt.s.w fa0, a0
        addi a0, zero, 2
        fcvt.s.w fa1, a0
        addi a0, zero, 3
        fcvt.s.w fa2, a0
        mv a0, sp
        jal ra, mk_vec3
        flw fa2, 0(sp)
        addi a0, zero, 1
        fcvt.s.w fa1, a0
        feq.s a0, fa2, fa1
        xori a0, a0, 1
        flw fa1, 4(sp)
        addi a1, zero, 2
        fcvt.s.w fa2, a1
        feq.s a1, fa1, fa2
        xori a1, a1, 1
        or a0, a0, a1
        flw fa2, 8(sp)
        addi a1, zero, 3
        fcvt.s.w fa1, a1
        feq.s a1, fa2, fa1
        xori a1, a1, 1
        or a0, a0, a1
        mv a1, zero
        beq a0, a1, .BB_2
        jal zero, .BB_1
.BB_1:
        addi a0, zero, 1
        addi sp, sp, 112
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0
.BB_2:
        addi a0, zero, 5
        addi a1, zero, 7
        jal ra, mk_small
        sw a0, 12(sp)
        sw a1, 16(sp)
        addi a1, sp, 12
        mv a0, zero
        addi a2, zero, 4
        mul a0, a0, a2
        add a1, a1, a0
        lw a1, 0(a1)
        addi a0, zero, 5
        bne a1, a0, .BB_4
        jal zero, .BB_3
.BB_3:
        addi a0, sp, 12
        addi a1, zero, 1
        addi a2, zero, 4
        mul a1, a1, a2
        add a0, a0, a1
        lw a0, 0(a0)
        addi a1, zero, 7
        bne a0, a1, .BB_5
        jal zero, .BB_7
.BB_4:
        jal zero, .BB_6
.BB_5:
        addi a1, zero, 1
        jal zero, .BB_8
.BB_6:
        addi a1, zero, 1
        jal zero, .BB_8
.BB_7:
        mv a1, zero
.BB_8:
        mv a0, zero
        beq a1, a0, .BB_10
        jal zero, .BB_9
.BB_9:
        addi a0, zero, 2
        addi sp, sp, 112
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0
.BB_10:
        addi a0, sp, 20
        jal ra, mk_big
        mv a0, zero
.BB_11:
        addi a1, zero, 16
        bge a0, a1, .BB_16
        jal zero, .BB_12
.BB_12:
        addi a1, sp, 20
        addi a2, zero, 4
        mul a2, a0, a2
        add a1, a1, a2
        lw a1, 0(a1)
        beq a1, a0, .BB_14
        jal zero, .BB_13
.BB_13:
        addi a0, zero, 3
        addi sp, sp, 112
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0
.BB_14:
        addi a1, zero, 1
        add a0, a0, a1
        jal zero, .BB_11
.BB_15:
        addi a1, zero, 1
        add a0, a0, a1
        jal zero, .BB_11
.BB_16:
        mv a0, zero
        addi sp, sp, 112
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0

