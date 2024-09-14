#include "graphcol.h"

#include "liveness.h"

// uses graph-coloring technique to allocate registers - relies on SSA form!
void graphcol_register_alloc(struct ir_builder *irb, struct reg_info reg_info) {
  UNUSED_ARG(irb);
  UNUSED_ARG(reg_info);

  todo("graph col");
}
