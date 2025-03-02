
main:
        sw ra, -4(sp)
        sw sp, -8(sp)
        addi sp, sp, -16
        lui a0, %hi(p_.str.2+0)
        addi a0, a0, %lo(p_.str.2+0)
        auipc ra, %pcrel_hi(puts+0)
        jalr ra, ra, %pcrel_lo(puts+0)
        mv a0, zero
        addi sp, sp, 16
        lw ra, -4(sp)
        lw sp, -8(sp)
        jalr zero, ra, 0


p_.str.2:
        .string "Hello, World!"