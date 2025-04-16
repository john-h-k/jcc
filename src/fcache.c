#include "fcache.h"

#include "alloc.h"
#include "hashtbl.h"
#include "profile.h"
#include "util.h"

struct fcache_key {
  enum f_ty { F_TY_PATH, F_TY_PROC, F_TY_STDIN } ty;
  ustr_t key;
};

struct fcache {
  struct arena_allocator *arena;
  enum fcache_flags flags;
  struct hashtbl *cache;
};

static void hash_fcache_key(struct hasher *hasher, const void *obj) {
  const struct fcache_key *value = obj;

  hasher_hash_integer(hasher, value->ty, sizeof(value->ty));
  hashtbl_hash_ustr(hasher, &value->key);
}

static bool eq_fcache_key(const void *l, const void *r) {
  const struct fcache_key *lc = l;
  const struct fcache_key *rc = r;

  return lc->ty == rc->ty && ustr_eq(lc->key, rc->key);
}

void fcache_create(struct arena_allocator *arena, enum fcache_flags flags,
                   struct fcache **fcache) {
  *fcache = arena_alloc(arena, sizeof(**fcache));

  **fcache = (struct fcache){
      .arena = arena,
      .flags = flags,
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

bool fcache_test_path(struct fcache *fcache, ustr_t path) {
  struct fcache_key key = {F_TY_PATH, .key = path};

  return fcache_read(fcache, key, NULL, FC_MODE_TEST);
}

bool fcache_read_stdin(struct fcache *fcache, struct fcache_file *file) {
  struct fcache_key key = {.ty = F_TY_STDIN, .key = MK_NULL_USTR()};

  return fcache_read(fcache, key, file, FC_MODE_READ);
}

bool fcache_read_path(struct fcache *fcache, ustr_t path,
                      struct fcache_file *file) {
  struct fcache_key key = {F_TY_PATH, .key = path};

  return fcache_read(fcache, key, file, FC_MODE_READ);
}

bool fcache_read_proc(struct fcache *fcache, ustr_t proc,
                      struct fcache_file *file) {
  struct fcache_key key = {F_TY_PROC, .key = proc};

  return fcache_read(fcache, key, file, FC_MODE_READ);
}

static void read_file_content(struct fcache *fcache, FILE *file, char **buf,
                              size_t *buf_len);

static bool fcache_read(struct fcache *fcache, struct fcache_key key,
                        struct fcache_file *file, enum fc_mode mode) {
  // TODO: caching (via mtime) even when flag not present
  if (fcache->flags & FCACHE_FLAG_ASSUME_CONSTANT) {
    struct fcache_file *cache = hashtbl_lookup(fcache->cache, &key);
    if (cache) {
      if (file) {
        *file = *cache;
      }
      return true;
    }
  }

  // FIXME: assert the sized_str has no null chars in it
  DEBUG_ASSERT(ustr_nullsafe(key.key), "fcache doesn't support null chars");

  PROFILE_CREATE_MULTI(fcache_read);
  PROFILE_BEGIN_MULTI(fcache_read);

  FILE *f;
  switch (key.ty) {
  case F_TY_PROC:
    // POSIX!! not C-std. we should have an alternatie
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
    PROFILE_END_MULTI(fcache_read);
    return false;
  }

  if (file) {
    *file = (struct fcache_file){
        .name = key.key,
    };
  }

  if (mode == FC_MODE_READ) {
    DEBUG_ASSERT(file, "file null");

    char *data;
    read_file_content(fcache, f, &data, &file->len);

    file->data = data;

    // clone becaue fcache may outlive callers
    key.key = arena_alloc_ustrdup(fcache->arena, key.key);
    file->name = arena_alloc_ustrdup(fcache->arena, file->name);

    hashtbl_insert(fcache->cache, &key, file);
  }

  // TODO: check return values
  switch (key.ty) {
  case F_TY_PROC:
    pclose(f);
    break;
  case F_TY_PATH:
    fclose(f);
    break;
  case F_TY_STDIN:
    break;
  }

  PROFILE_END_MULTI(fcache_read);
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

    content[len] = '\0';

    *buf = content;
    *buf_len = len;
    return;
  }

  rewind(file);

  char *content = arena_alloc(fcache->arena, (unsigned long)fsize + 1);
  size_t read = fread(content, 1, (unsigned long)fsize, file);

  if (read != (size_t)fsize) {
    *buf = NULL;
    *buf_len = 0;
  }

  content[fsize] = '\0';
  *buf = content;
  *buf_len = (size_t)(fsize + 1);
}
