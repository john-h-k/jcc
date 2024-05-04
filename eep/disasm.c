#include "disasm.h"

#include <stdint.h>

#include "../util.h"
#include "../log.h"
#include "../bit_twiddle.h"

static void disasm_instr(const char *instr);

void eep_debug_disasm(const char *filename) {
  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    err("failed to open file for disasm");
    return;
  }

  char *line = NULL;
  size_t len = 0;
  ssize_t read;

  while ((read = getline(&line, &len, file)) != -1) {
    disasm_instr(line);
  }

  fclose(file);
}

#define A(instr) (((instr) >> 9) & 0b111)
#define B(instr) (((instr) >> 5) & 0b111)
#define C(instr) (((instr) >> 2) & 0b111)

const char *SHIFT_OPC_NAMES[] = { "LSL", "LSR", "ASR", "XSR" };
const char *ALU_OPC_NAMES[] = { "MOV", "ADD", "SUB", "ADC", "SBC", "AND", "CMP", "Shift" /* shouldn't be hit, keep it for safety though */ };

const char *JUMP_OPC_NAMES[] = { "JMP", "JEQ", "JCS", "JMI", "JGE", "JGT", "JHI", "JSR" };
const char *NEG_JUMP_OPC_NAMES[] = { "NOOP", "JNE", "JCC", "JPL", "JLT", "JLE", "JLS", "RET" };

static void disasm_instr(const char *line) {
  int idx;
  int wd_instr;

  if (sscanf(line, "%x %x", &idx, &wd_instr) != 2) {
    warn("failed to parse line in EEP disasm");
    return;
  }

  uint16_t instr = (uint16_t)wd_instr;

  if (NTH_BIT(instr, 15)) {
    // not ALU

    unsigned opc = instr >> 12;
    if (opc == 0b1100) {
      // jump
      unsigned jump_opc = (instr >> 9) & 0b111;
      unsigned offset = instr & 0b11111111;
      if (NTH_BIT(instr, 8)) {
        // negated

        if (jump_opc == 7) {
          // RET
          printf("RET");
        } else {
          printf("%s %d", NEG_JUMP_OPC_NAMES[jump_opc], offset);
        }
      } else {
        printf("%s %d", JUMP_OPC_NAMES[jump_opc], offset);
      }
    } else if (opc == 0b1101) {
      // ext
      unsigned imm = instr & 0b11111111;
      printf("EXT %d", imm);
    } else {
      // unused
      printf("UNDEF.");
    }
  } else {
    // ALU instr

    uint16_t alu_opc = instr >> 12;

    if (alu_opc > ARR_LENGTH(ALU_OPC_NAMES)) {
      err("invalid aluopc field");
      return;
    }

    if (alu_opc == 7) {
      // shift
      unsigned a = A(instr);
      unsigned b = B(instr);

      unsigned shift_opc = (NTH_BIT(instr, 8) << 1) | NTH_BIT(instr, 4);
      unsigned scnt = instr & 0b1111;

      if (shift_opc > ARR_LENGTH(SHIFT_OPC_NAMES)) {
        err("invalid shiftopc field");
        return;
      }

      printf("%s R%d, R%d, #%d", SHIFT_OPC_NAMES[shift_opc], a, b, scnt);
    } else if (NTH_BIT(instr, 8)) {
      // immediate
      int a = A(instr);
      int imm = instr & 0b11111111;

      printf("%s R%d, #%d", ALU_OPC_NAMES[alu_opc], a, imm);
    } else {
      // two reg
      int a = A(instr);
      int b = B(instr);
      int c = C(instr);

      if (alu_opc == 0 || alu_opc == 6) {
        // MOV and CMP don't use c
        printf("%s R%d, R%d", ALU_OPC_NAMES[alu_opc], a, b);
      } else {
        printf("%s R%d, R%d, R%d", ALU_OPC_NAMES[alu_opc], c, a, b);
      }
    }
  }

  printf("\n");
}
