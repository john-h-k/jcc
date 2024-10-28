#ifndef PREPROC_H
#define PREPROC_H

#include "program.h"

enum preproc_create_result { PREPROC_CREATE_RESULT_SUCCESS = 0 };

struct preproc;

enum preproc_create_result preproc_create(struct program *program, size_t num_include_paths, const char **include_paths,
                                          struct preproc **preproc);
struct preprocessed_program preproc_process(struct preproc *preproc);
void preproc_free(struct preproc **preproc);

#endif
