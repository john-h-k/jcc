#include "fcache.h"

#include "alloc.h"
#include "hashtbl.h"
#include "util.h"

struct fcache_key {
  enum f_ty { F_TY_PATH, F_TY_PROC, F_TY_STDIN } ty;
  struct sized_str key;
};

struct fcache {
  struct arena_allocator *arena;
  struct hashtbl *cache;
};

static void hash_fcache_key(struct hasher *hasher, const void *obj) {
  const struct fcache_key *value = obj;

  hasher_hash_integer(hasher, value->ty, sizeof(value->ty));
  hashtbl_hash_sized_str(hasher, &value->key);
}

static bool eq_fcache_key(const void *l, const void *r) {
  const struct fcache_key *lc = l;
  const struct fcache_key *rc = r;

  return lc->ty == rc->ty && szstreq(lc->key, rc->key);
}

void fcache_create(struct arena_allocator *arena, struct fcache **fcache) {
  *fcache = arena_alloc(arena, sizeof(**fcache));

  **fcache = (struct fcache){
      .arena = arena,
      .cache = hashtbl_create_in_arena(arena, sizeof(struct fcache_key),
                                       sizeof(struct fcache_file),
                                       hash_fcache_key, eq_fcache_key)};
}

enum fc_mode {
  FC_MODE_READ,
  FC_MODE_TEST,
};

static bool fcache_read(struct fcache *fcache, struct fcache_key key,
                        struct fcache_file *file, enum fc_mode mode);

bool fcache_test_path(struct fcache *fcache, struct sized_str path) {
  struct fcache_key key = {F_TY_PATH, .key = path};

  return fcache_read(fcache, key, NULL, FC_MODE_TEST);
}

bool fcache_read_stdin(struct fcache *fcache, struct fcache_file *file) {
  struct fcache_key key = {.ty = F_TY_STDIN, .key = MK_NULL_STR()};

  return fcache_read(fcache, key, file, FC_MODE_READ);
}

bool fcache_read_path(struct fcache *fcache, struct sized_str path,
                      struct fcache_file *file) {
  struct fcache_key key = {F_TY_PATH, .key = path};

  return fcache_read(fcache, key, file, FC_MODE_READ);
}

bool fcache_read_proc(struct fcache *fcache, struct sized_str proc,
                      struct fcache_file *file) {
  struct fcache_key key = {F_TY_PROC, .key = proc};

  return fcache_read(fcache, key, file, FC_MODE_READ);
}

static void read_file_content(struct fcache *fcache, FILE *file, char **buf,
                              size_t *buf_len);

static bool fcache_read(struct fcache *fcache, struct fcache_key key,
                        struct fcache_file *file, enum fc_mode mode) {
  struct fcache_file *cache = hashtbl_lookup(fcache->cache, &key);
  if (cache) {
    if (file) {
      *file = *cache;
    }
    return true;
  }

  // FIXME: assert the sized_str has no null chars in it
  FILE *f;
  switch (key.ty) {
  case F_TY_PROC:
    f = popen(key.key.str, "r");
    break;
  case F_TY_PATH:
    f = fopen(key.key.str, "r");
    break;
  case F_TY_STDIN:
    f = stdin;
    break;
  }

  if (!f) {
    return false;
  }

  if (file) {
    *file = (struct fcache_file){
        .name = key.key,
    };
  }

  if (mode == FC_MODE_READ) {
    char *data;
    read_file_content(fcache, f, &data, &file->len);

    file->data = data;

    hashtbl_insert(fcache->cache, &key, file);
  }

  return true;
}

static void read_file_content(struct fcache *fcache, FILE *file, char **buf,
                              size_t *buf_len) {
  fseek(file, 0, SEEK_END);
  long fsize = ftell(file);

  if (fsize == -1L) {
// can't tell size of file (e.g stdin), just iteratively read instead
#define READ_BUF_SZ (4096)

    size_t read = 0;
    size_t len = 0;

    char *content = NULL;
    char *head;

    do {
      content = arena_realloc(fcache->arena, content, len + READ_BUF_SZ);
      head = &content[len];

      len += read;

    } while ((read = fread(head, 1, READ_BUF_SZ, file)) > 0);

    fclose(file);

    content[len] = '\0';

    *buf = content;
    *buf_len = len;
    return;
  }

  rewind(file);

  char *content = arena_alloc(fcache->arena, (unsigned long)fsize + 1);
  size_t read = fread(content, 1, (unsigned long)fsize, file);
  fclose(file);

  if (read != (size_t)fsize) {
    *buf = NULL;
    *buf_len = 0;
  }

  content[fsize] = '\0';
  *buf = content;
  *buf_len = (size_t)(fsize + 1);
}
