#ifndef EEP_ISA_H
#define EEP_ISA_H

#define EEP_REG_SIZE (2)
#define EEP_INSTR_SIZE (2)

#define ALU_SHIFT(a, shift_opc1, b, shift_opc0, imm4) \
  (uint16_t)((7 << 12) | ((a) << 9) | ((shift_opc1) << 8) | ((b) << 5) | ((shift_opc0) << 4) | (imm4))

#define ALU(aluopc, a, b, c) \
  (uint16_t)(((aluopc) << 12) | ((a) << 9) | ((b) << 5) | ((c) << 2))

#define ALU_IMM(aluopc, a, imms8) \
  (uint16_t)(((aluopc) << 12) | ((a) << 9) | (1 << 8) | (imms8))

#define MOV(Ra, Rb) ALU(0, Ra, Rb, 0)
#define MOV_IMM(Ra, imm) ALU_IMM(0, Ra, imm)

#define ADD(Rc, Ra, Rb) ALU(1, Ra, Rb, Rc)
#define SUB(Rc, Ra, Rb) ALU(2, Ra, Rb, Rc)
#define ADC(Rc, Ra, Rb) ALU(3, Ra, Rb, Rc)
#define SBC(Rc, Ra, Rb) ALU(4, Ra, Rb, Rc)
#define AND(Rc, Ra, Rb) ALU(5, Ra, Rb, Rc)
#define CMP(Ra, Rb) ALU(1, Ra, Rb, 0)

#define ADD_IMM(Ra, imm) ALU_IMM(1, Ra, imm)
#define SUB_IMM(Ra, imm) ALU_IMM(2, Ra, imm)
#define ADC_IMM(Ra, imm) ALU_IMM(3, Ra, imm)
#define SBC_IMM(Ra, imm) ALU_IMM(4, Ra, imm)
#define AND_IMM(Ra, imm) ALU_IMM(5, Ra, imm)
#define CMP_IMM(Ra, imm) ALU(1, Ra, imm, 0)

#define LSL(Ra, Rb, scnt) ALU_SHIFT(Ra, 0, Rb, 0, scnt)
#define LSR(Ra, Rb, scnt) ALU_SHIFT(Ra, 0, Rb, 1, scnt)
#define ASR(Ra, Rb, scnt) ALU_SHIFT(Ra, 1, Rb, 0, scnt)
#define XSR(Ra, Rb, scnt) ALU_SHIFT(Ra, 1, Rb, 1, scnt)

#define LDR_STR_DIRECT(mopc, a, imm) \
  (uint16_t)((1 << 15) | ((mopc) << 13) | ((a) << 9) | (1 << 8) | (imm))

#define LDR_STR_OFFSET(mopc, a, b, offset) \
  (uint16_t)((1 << 15) | ((mopc) << 13) | ((a) << 9) | ((b) << 5) | (offset))

#define LDR_DIRECT(Ra, imm) LDR_STR_DIRECT(0, Ra, imm)
#define STR_DIRECT(Ra, imm) LDR_STR_DIRECT(1, Ra, imm)

#define LDR_OFFSET(Ra, Rb, offset) LDR_STR_OFFSET(0, Ra, Rb, offset)
#define STR_OFFSET(Ra, Rb, offset) LDR_STR_OFFSET(1, Ra, Rb, offset)

#define JUMP(jmpopc, offset) \
  (uint16_t)((0b11 << 14) | ((jmpopc) << 8) | (uint8_t)(offset))

#define EXT(imm) \
  (uint16_t)((0b1101 << 12) | (imm))

#endif
