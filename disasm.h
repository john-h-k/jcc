#ifndef DISASM_H
#define DISASM_H

// dumps using `objdump -d `, works for macOS
void objdump_debug_disasm(const char *filename, const char *output);

#endif
