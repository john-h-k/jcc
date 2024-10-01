#include "codegen.h"

#include "../util.h"

#include <stdio.h>

void aarch64_codegen(const struct ir_builder *irb) {
  struct codegen_function func = {.ty = CODEGEN_FUNCTION_TY_AARCH64,
                                  .first = NULL,
                                  .last = NULL,
                                  .instr_count = 0,
                                  .instr_size = sizeof(struct aarch64_instr)};

  arena_allocator_create(&func.arena);

  struct instr *instr = alloc_instr(&func);
  instr->aarch64->ty = AARCH64_INSTR_TY_AND_32;
  instr->aarch64->logical_reg = (struct aarch64_logical_reg){
      .lhs = (struct aarch64_reg){AARCH64_REG_TY_W, 0},
      .rhs = (struct aarch64_reg){AARCH64_REG_TY_W, 1},
      .dest = (struct aarch64_reg){AARCH64_REG_TY_W, 6},
      .imm6 = 6,
      .shift = AARCH64_SHIFT_ASR,
  };

  debug_print_func(stderr, &func);
}

char reg_prefix(struct aarch64_reg reg) {
  switch (reg.ty) {
  case AARCH64_REG_TY_W:
    return 'w';
  case AARCH64_REG_TY_X:
    return 'w';
  case AARCH64_REG_TY_V:
    return 'b';
  case AARCH64_REG_TY_Q:
    return 'q';
  case AARCH64_REG_TY_D:
    return 'd';
  case AARCH64_REG_TY_S:
    return 's';
  case AARCH64_REG_TY_H:
    return 'h';
  case AARCH64_REG_TY_B:
    return 'b';
  }
}

void codegen_fprintf(FILE *file, const char *format, ...) {
  va_list list;
  va_start(list, format);
  while (format[0] != '\0') {
    if (format[0] != '%') {
      fputc(format[0], file);
      format++;
      continue;
    }

    format++;

    if (strncmp(format, "addr_imm", 8) == 0) {
      // expects addressing mode + register + immediate
      enum aarch64_addressing_mode addressing_mode =
          va_arg(list, enum aarch64_addressing_mode);
      struct aarch64_reg reg = va_arg(list, struct aarch64_reg);
      size_t imm = va_arg(list, size_t);

      char prefix = reg_prefix(reg);

      if (!imm) {
        fprintf(file, "[%c%zu]", prefix, reg.idx);
      } else {
        switch (addressing_mode) {
        case AARCH64_ADDRESSING_MODE_OFFSET:
          fprintf(file, "[%c%zu, #%zu]", prefix, reg.idx, imm);
          break;
        case AARCH64_ADDRESSING_MODE_PREINDEX:
          fprintf(file, "[%c%zu, #%zu]!", prefix, reg.idx, imm);
          break;
        case AARCH64_ADDRESSING_MODE_POSTINDEX:
          fprintf(file, "[%c%zu], #%zu", prefix, reg.idx, imm);
          break;
        }
      }

      format += 8;
    } else if (strncmp(format, "shift_imm", 9) == 0) {
      // expects a shift + an immediate, and prints if shift != LSL or imm != 0
      enum aarch64_shift shift = va_arg(list, enum aarch64_shift);
      size_t imm = va_arg(list, size_t);

      if (shift != AARCH64_SHIFT_LSL || imm) {
        switch (shift) {
        case AARCH64_SHIFT_LSL:
          fprintf(file, ", lsl #%zu", imm);
          break;
        case AARCH64_SHIFT_LSR:
          fprintf(file, ", lsr #%zu", imm);
          break;
        case AARCH64_SHIFT_ASR:
          fprintf(file, ", asr #%zu", imm);
          break;
        case AARCH64_SHIFT_RESERVED:
          fprintf(file, ", RESERVED");
          break;
        }
      }

      format += 9;
    } else if (strncmp(format, "log_imm", 7) == 0) {
      // logical immediate are weird
      size_t n = va_arg(list, size_t);
      size_t immr = va_arg(list, size_t);
      size_t imms = va_arg(list, size_t);
      if (!n && !immr && !imms) {
        fprintf(file, "#0");
      } else {
        todo("logical immediates");
      }

      format += 7;
    } else if (strncmp(format, "reg", 3) == 0) {
      struct aarch64_reg reg = va_arg(list, struct aarch64_reg);
      char prefix = reg_prefix(reg);
      fputc(prefix, file);
      fprintf(file, "%zu", reg.idx);

      format += 3;
    } else if (strncmp(format, "instr", 5) == 0) {
      struct instr *instr = va_arg(list, struct instr *);
      fprintf(file, "%%%zu", instr->id);

      format += 5;
    } else if (strncmp(format, "cond", 4) == 0) {
      enum aarch64_cond cond = va_arg(list, enum aarch64_cond);
      switch (cond) {
      case AARCH64_COND_AL:
        fprintf(file, "al");
        break;
      case AARCH64_COND_AL_ALT:
        fprintf(file, "alt");
        break;
      case AARCH64_COND_EQ:
        fprintf(file, "eq");
        break;
      case AARCH64_COND_NE:
        fprintf(file, "ne");
        break;
      case AARCH64_COND_GE:
        fprintf(file, "ge");
        break;
      case AARCH64_COND_LT:
        fprintf(file, "lt");
        break;
      case AARCH64_COND_GT:
        fprintf(file, "gt");
        break;
      case AARCH64_COND_LE:
        fprintf(file, "le");
        break;
      case AARCH64_COND_VS:
        fprintf(file, "vs");
        break;
      case AARCH64_COND_VC:
        fprintf(file, "vc");
        break;
      case AARCH64_COND_CS:
        fprintf(file, "cs");
        break;
      case AARCH64_COND_CC:
        fprintf(file, "cc");
        break;
      case AARCH64_COND_HI:
        fprintf(file, "hi");
        break;
      case AARCH64_COND_LS:
        fprintf(file, "ls");
        break;
      case AARCH64_COND_MI:
        fprintf(file, "mi");
        break;
      case AARCH64_COND_PL:
        fprintf(file, "pl");
        break;
        // prints the other form (CS/CC)
        // case AARCH64_COND_HS:
        //   fprintf(file, "hs");
        //   break;
        // case AARCH64_COND_LO:
        //   fprintf(file, "lo");
        //   break;
      }

      format += 4;
    } else if (strncmp(format, "shift", 5) == 0) {
      enum aarch64_shift shift = va_arg(list, enum aarch64_shift);
      switch (shift) {
      case AARCH64_SHIFT_LSL:
        fprintf(file, "lsl");
        break;
      case AARCH64_SHIFT_LSR:
        fprintf(file, "lsr");
        break;
      case AARCH64_SHIFT_ASR:
        fprintf(file, "asr");
        break;
      case AARCH64_SHIFT_RESERVED:
        fprintf(file, "RESERVED");
        break;
      }

      format += 5;
    } else if (strncmp(format, "imm", 3) == 0) {
      size_t imm = va_arg(list, size_t);
      fprintf(file, "#%zu", imm);

      format += 3;
    } else if (format[0] == '%') {
      fputc('%', file);
      format++;
    } else {
      bug("unrecognised format starting '%%%s'", format);
    }
  }
}

void debug_print_logical_reg(FILE *file,
                             const struct aarch64_logical_reg *logical_reg) {
  codegen_fprintf(file, " %reg, %reg, %reg%shift_imm", logical_reg->dest,
                  logical_reg->lhs, logical_reg->rhs, logical_reg->shift,
                  logical_reg->imm6);
}

void debug_print_logical_imm(FILE *file,
                             const struct aarch64_logical_imm *logical_imm) {
  codegen_fprintf(file, " %reg, %reg, %log_imm", logical_imm->dest,
                  logical_imm->source, logical_imm->n, logical_imm->immr,
                  logical_imm->imms);
}

void debug_print_addr_imm(FILE *file, const struct aarch64_addr_imm *addr_imm) {
  codegen_fprintf(file, " %reg, %imm", addr_imm->dest, addr_imm->imm);
}

void debug_print_addsub_reg(FILE *file,
                            const struct aarch64_addsub_reg *addsub_reg) {
  codegen_fprintf(file, " %reg, %reg, %reg%shift_imm", addsub_reg->dest,
                  addsub_reg->lhs, addsub_reg->rhs, addsub_reg->shift,
                  addsub_reg->imm6);
}

void debug_print_addsub_imm(FILE *file,
                            const struct aarch64_addsub_imm *addsub_imm) {
  if (addsub_imm->shift) {
    codegen_fprintf(file, " %reg, %reg, %imm, lsl %imm", addsub_imm->dest,
                    addsub_imm->source, addsub_imm->imm, addsub_imm->shift);
  } else {
    codegen_fprintf(file, " %reg, %reg, %imm", addsub_imm->dest,
                    addsub_imm->source, addsub_imm->imm);
  }
}

void debug_print_reg_1_source(FILE *file,
                              const struct aarch64_reg_1_source *reg_1_source) {
  codegen_fprintf(file, " %reg, %reg", reg_1_source->dest,
                  reg_1_source->source);
}

void debug_print_reg_2_source(FILE *file,
                              const struct aarch64_reg_2_source *reg_2_source) {
  codegen_fprintf(file, " %reg, %reg, %reg", reg_2_source->dest,
                  reg_2_source->lhs, reg_2_source->rhs);
}

void debug_print_bitfield_imm(FILE *file,
                              const struct aarch64_bitfield_imm *bitfield_imm) {
  codegen_fprintf(file, " %reg, %reg, %imm, %imm", bitfield_imm->dest,
                  bitfield_imm->source, bitfield_imm->immr, bitfield_imm->imms);
}

void debug_print_conditional_select(
    FILE *file, const struct aarch64_conditional_select *conditional_select) {
  codegen_fprintf(file, "% reg, %reg, %reg, %cond", conditional_select->dest,
                  conditional_select->false_source,
                  conditional_select->true_source, conditional_select->cond);
}

void debug_print_conditional_branch(
    FILE *file, const struct aarch64_conditional_branch *conditional_branch) {
  codegen_fprintf(file, ".%cond %instr", conditional_branch->target,
                  conditional_branch->cond);
}

void debug_print_branch(FILE *file, const struct aarch64_branch *branch) {
  codegen_fprintf(file, " %instr", branch->target);
}

void debug_print_ret(FILE *file, const struct aarch64_ret *ret) {
  codegen_fprintf(file, " %reg", ret->target);
}

void debug_print_compare_and_branch(
    FILE *file, const struct aarch64_compare_and_branch *compare_and_branch) {
  codegen_fprintf(file, " %reg, %instr", compare_and_branch->cmp,
                  compare_and_branch->target);
}

void debug_print_loadstore_imm(
    FILE *file, const struct aarch64_loadstore_imm *loadstore_imm) {
  codegen_fprintf(file, " %reg, %addr_imm", loadstore_imm->dest,
                  loadstore_imm->mode, loadstore_imm->source, loadstore_imm->imm);
}

void debug_print_loadstore_pair_imm(
    FILE *file, const struct aarch64_loadstore_pair_imm *loadstore_pair_imm) {
  codegen_fprintf(file, " %reg, %reg, %addr_imm", loadstore_pair_imm->dest[0], loadstore_pair_imm->dest[1],
                  loadstore_pair_imm->mode, loadstore_pair_imm->source, loadstore_pair_imm->imm);
}

void debug_print_mov_imm(FILE *file,
                              const struct aarch64_mov_imm *mov_imm) {
  if (mov_imm->shift) {
    codegen_fprintf(file, " %reg, %imm, lsl %imm", mov_imm->dest, mov_imm->imm, mov_imm->shift);
  } else {
    codegen_fprintf(file, " %reg, %imm", mov_imm->dest, mov_imm->imm);
  }
}


void debug_print_instr(FILE *file, const struct codegen_function *func,
                       const struct instr *instr) {

  switch (instr->aarch64->ty) {
  case AARCH64_INSTR_TY_NOP:
    fprintf(file, "nop");
    break;
  case AARCH64_INSTR_TY_SBFM_32_IMM:
  case AARCH64_INSTR_TY_SBFM_64_IMM:
    fprintf(file, "sbfm");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_BFM_32_IMM:
  case AARCH64_INSTR_TY_BFM_64_IMM:
    fprintf(file, "bfm");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_UBFM_32_IMM:
  case AARCH64_INSTR_TY_UBFM_64_IMM:
    fprintf(file, "ubfm");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_AND_32:
  case AARCH64_INSTR_TY_AND_64:
    fprintf(file, "and");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_ANDS_32:
  case AARCH64_INSTR_TY_ANDS_64:
    fprintf(file, "ands");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_ORR_32:
  case AARCH64_INSTR_TY_ORR_64:
    fprintf(file, "orr");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_ORN_32:
  case AARCH64_INSTR_TY_ORN_64:
    fprintf(file, "orn");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_EOR_32:
  case AARCH64_INSTR_TY_EOR_64:
    fprintf(file, "eor");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_EON_32:
  case AARCH64_INSTR_TY_EON_64:
    fprintf(file, "eon");
    debug_print_logical_reg(file, &instr->aarch64->logical_reg);
    break;
  case AARCH64_INSTR_TY_EOR_32_IMM:
  case AARCH64_INSTR_TY_EOR_64_IMM:
    fprintf(file, "eor");
    debug_print_logical_imm(file, &instr->aarch64->logical_imm);
    break;
  case AARCH64_INSTR_TY_ORR_32_IMM:
  case AARCH64_INSTR_TY_ORR_64_IMM:
    fprintf(file, "orr");
    debug_print_logical_imm(file, &instr->aarch64->logical_imm);
    break;
  case AARCH64_INSTR_TY_ANDS_32_IMM:
  case AARCH64_INSTR_TY_ANDS_64_IMM:
    fprintf(file, "ands");
    debug_print_logical_imm(file, &instr->aarch64->logical_imm);
    break;
  case AARCH64_INSTR_TY_AND_32_IMM:
  case AARCH64_INSTR_TY_AND_64_IMM:
    fprintf(file, "and");
    debug_print_logical_imm(file, &instr->aarch64->logical_imm);
    break;
  case AARCH64_INSTR_TY_ADDS_32:
  case AARCH64_INSTR_TY_ADDS_64:
    fprintf(file, "adds");
    debug_print_addsub_reg(file, &instr->aarch64->addsub_reg);
    break;
  case AARCH64_INSTR_TY_ADD_32:
  case AARCH64_INSTR_TY_ADD_64:
    fprintf(file, "add");
    debug_print_addsub_reg(file, &instr->aarch64->addsub_reg);
    break;
  case AARCH64_INSTR_TY_ADD_32_IMM:
  case AARCH64_INSTR_TY_ADD_64_IMM:
    fprintf(file, "add");
    debug_print_addsub_imm(file, &instr->aarch64->addsub_imm);
    break;
  case AARCH64_INSTR_TY_ADR:
    fprintf(file, "adr");
    debug_print_addr_imm(file, &instr->aarch64->addr_imm);
    break;
  case AARCH64_INSTR_TY_ADRP:
    fprintf(file, "adrp");
    debug_print_addr_imm(file, &instr->aarch64->addr_imm);
    break;
  case AARCH64_INSTR_TY_ASRV_32:
  case AARCH64_INSTR_TY_ASRV_64:
    fprintf(file, "asrv");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  case AARCH64_INSTR_TY_B:
    fprintf(file, "b");
    debug_print_branch(file, &instr->aarch64->branch);
    break;
  case AARCH64_INSTR_TY_BL:
    fprintf(file, "bl");
    debug_print_branch(file, &instr->aarch64->branch);
    break;
  case AARCH64_INSTR_TY_BC_COND:
    fprintf(file, "bc.");
    debug_print_conditional_branch(file, &instr->aarch64->conditional_branch);
    break;
  case AARCH64_INSTR_TY_B_COND:
    fprintf(file, "b.");
    debug_print_conditional_branch(file, &instr->aarch64->conditional_branch);
    break;
  case AARCH64_INSTR_TY_CBNZ_32_IMM:
  case AARCH64_INSTR_TY_CNBZ_64_IMM:
    fprintf(file, "cbnz");
    debug_print_compare_and_branch(file, &instr->aarch64->compare_and_branch);
    break;
  case AARCH64_INSTR_TY_CBZ_32_IMM:
  case AARCH64_INSTR_TY_CBZ_64_IMM:
    fprintf(file, "cbz");
    debug_print_compare_and_branch(file, &instr->aarch64->compare_and_branch);
    break;
  case AARCH64_INSTR_TY_CSEL_32:
  case AARCH64_INSTR_TY_CSEL_64:
    fprintf(file, "csel");
    debug_print_conditional_select(file, &instr->aarch64->conditional_select);
    break;
  case AARCH64_INSTR_TY_CSINC_32:
  case AARCH64_INSTR_TY_CSINC_64:
    fprintf(file, "csinc");
    debug_print_conditional_select(file, &instr->aarch64->conditional_select);
    break;
  case AARCH64_INSTR_TY_CSINV_32:
  case AARCH64_INSTR_TY_CSINV_64:
    fprintf(file, "csinv");
    debug_print_conditional_select(file, &instr->aarch64->conditional_select);
    break;
  case AARCH64_INSTR_TY_CSNEG_32:
  case AARCH64_INSTR_TY_CSNEG_64:
    fprintf(file, "csneg");
    debug_print_conditional_select(file, &instr->aarch64->conditional_select);
    break;
  case AARCH64_INSTR_TY_LOAD_32_IMM:
  case AARCH64_INSTR_TY_LOAD_64_IMM:
    fprintf(file, "ldr");
    debug_print_loadstore_imm(file, &instr->aarch64->loadstore_imm);
    break;
  case AARCH64_INSTR_TY_LOAD_PAIR_32_IMM:
  case AARCH64_INSTR_TY_LOAD_PAIR_64_IMM:
    fprintf(file, "ldp");
    debug_print_loadstore_pair_imm(file, &instr->aarch64->loadstore_pair_imm);
    break;
  case AARCH64_INSTR_TY_LSLV_32:
  case AARCH64_INSTR_TY_LSLV_64:
    fprintf(file, "lslv");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  case AARCH64_INSTR_TY_LSRV_32:
  case AARCH64_INSTR_TY_LSRV_64:
    fprintf(file, "lsrv");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  case AARCH64_INSTR_TY_MADD_32:
  case AARCH64_INSTR_TY_MADD_64:
    fprintf(file, "madd");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  case AARCH64_INSTR_TY_MOVN_32_IMM:
  case AARCH64_INSTR_TY_MOVN_64_IMM:
    fprintf(file, "movn");
    debug_print_mov_imm(file, &instr->aarch64->mov_imm);
    break;
  case AARCH64_INSTR_TY_MOV_32_IMM:
  case AARCH64_INSTR_TY_MOV_64_IMM:
    fprintf(file, "mov");
    debug_print_mov_imm(file, &instr->aarch64->mov_imm);
    break;
  case AARCH64_INSTR_TY_MOV_32:
  case AARCH64_INSTR_TY_MOV_64:
    fprintf(file, "mov");
    debug_print_reg_1_source(file, &instr->aarch64->reg_1_source);
    break;
  case AARCH64_INSTR_TY_MSUB_32:
  case AARCH64_INSTR_TY_MSUB_64:
    fprintf(file, "msub");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  case AARCH64_INSTR_TY_RET:
    fprintf(file, "ret");
    debug_print_ret(file, &instr->aarch64->ret);
    break;
  case AARCH64_INSTR_TY_RORV_32:
  case AARCH64_INSTR_TY_RORV_64:
    fprintf(file, "rorv");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  case AARCH64_INSTR_TY_SDIV_32:
  case AARCH64_INSTR_TY_SDIV_64:
    fprintf(file, "sdiv");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  case AARCH64_INSTR_TY_STORE_32_IMM:
  case AARCH64_INSTR_TY_STORE_64_IMM:
    fprintf(file, "str");
    debug_print_loadstore_imm(file, &instr->aarch64->loadstore_imm);
    break;
  case AARCH64_INSTR_TY_STORE_PAIR_32_IMM:
  case AARCH64_INSTR_TY_STORE_PAIR_64_IMM:
    fprintf(file, "stp");
    debug_print_loadstore_pair_imm(file, &instr->aarch64->loadstore_pair_imm);
    break;
  case AARCH64_INSTR_TY_SUBS_32:
  case AARCH64_INSTR_TY_SUBS_64:
    fprintf(file, "subs");
    debug_print_addsub_reg(file, &instr->aarch64->addsub_reg);
    break;
  case AARCH64_INSTR_TY_SUB_32:
  case AARCH64_INSTR_TY_SUB_64:
    fprintf(file, "sub");
    debug_print_addsub_reg(file, &instr->aarch64->addsub_reg);
    break;
  case AARCH64_INSTR_TY_SUB_32_IMM:
  case AARCH64_INSTR_TY_SUB_64_IMM:
    fprintf(file, "sub");
    debug_print_addsub_imm(file, &instr->aarch64->addsub_imm);
    break;
  case AARCH64_INSTR_TY_UDIV_32:
  case AARCH64_INSTR_TY_UDIV_64:
    fprintf(file, "udiv");
    debug_print_reg_2_source(file, &instr->aarch64->reg_2_source);
    break;
  }
}

void debug_print_func(FILE *file, const struct codegen_function *func) {
  debug_assert(func->ty == CODEGEN_FUNCTION_TY_AARCH64, "expected aarch64");

  int offset = 0;
  struct instr *instr = func->first;
  while (instr) {
    fprintf(file, "0x%04X: ", offset++);
    debug_print_instr(file, func, instr);
    fprintf(file, "\n");


    instr = instr->succ;
  }
}
