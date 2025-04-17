#ifndef IR_VALIDATE_H
#define IR_VALIDATE_H

#include "ir.h"

enum FLAG_ENUM ir_validate_flags {
  IR_VALIDATE_FLAG_NONE = 0,
  IR_VALIDATE_FLAG_ALLOW_MIXED_PHIS = 1,
  IR_VALIDATE_FLAG_LOWERED_POINTERS = 2,
};

void ir_validate(struct ir_unit *iru, enum ir_validate_flags flags);

#endif
