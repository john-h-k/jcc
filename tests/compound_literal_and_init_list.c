
struct instr {
  struct aarch64 *aarch64;
};

struct aarch64_reg {
  int ty;

  unsigned long idx;
};

struct aarch64_load_imm {
  int mode, imm;
  struct aarch64_reg dest[2], addr;
};

struct aarch64 {
  int ty;
  union {
    union {
      int a, b;
    };

    union {
      struct aarch64_load_imm load, ldr_imm;
    };
  };
};

unsigned long translate_reg_idx(unsigned long idx, int ty) { return idx; }

int main() {
  struct instr inst;
  struct instr *restore = &inst;
  restore->aarch64->ty = 1;
  restore->aarch64->ldr_imm = (struct aarch64_load_imm){
      .mode = 45,
      .imm = 3,
      .dest = {(struct aarch64_reg){.ty = 1, .idx = translate_reg_idx(10, 10)},
               (struct aarch64_reg){.ty = 1, .idx = translate_reg_idx(10, 10)}},
      .addr = ((struct aarch64_reg){1, 31}),
  };
}
