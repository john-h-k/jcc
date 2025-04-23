#include "fs.h"

#include "alloc.h"
#include "hashtbl.h"
#include "profile.h"
#include "util.h"
#include "thrd.h"
#include "vector.h"
#include <stdlib.h>
#include <string.h>

// TODO: non-libc
#include <unistd.h>
#include <errno.h>

struct fs_key {
  enum f_ty { F_TY_PATH, F_TY_PROC, F_TY_STDIN } ty;
  ustr_t key;
};

struct fs {
  struct arena_allocator *arena;
  enum fs_flags flags;
  struct hashtbl *cache;
};

static void hash_fs_key(struct hasher *hasher, const void *obj) {
  const struct fs_key *value = obj;

  hasher_hash_integer(hasher, value->ty, sizeof(value->ty));
  hashtbl_hash_ustr(hasher, &value->key);
}

static bool eq_fs_key(const void *l, const void *r) {
  const struct fs_key *lc = l;
  const struct fs_key *rc = r;

  return lc->ty == rc->ty && ustr_eq(lc->key, rc->key);
}

static struct {
  struct vector *tmp_names;
  ustr_t tmp_dir;
} TMP_INFO;

static mtx_t TMP_INFO_LOCK;
static once_flag TMP_INFO_FLAG = ONCE_FLAG_INIT;

static void fs_tmp_del(void) {
  MTX_LOCK(&TMP_INFO_LOCK, {
    while (vector_length(TMP_INFO.tmp_names)) {
      char *name = *(char **)vector_pop(TMP_INFO.tmp_names);

      unlink(name);
      free(name);
    }
  });
}

// at_quick_exit doesn't seem to exist?
#if defined(__APPLE__) && defined(__MAC_OS_X_VERSION_MAX_ALLOWED)
  #if __MAC_OS_X_VERSION_MAX_ALLOWED < 130000
  static void at_quick_exit(UNUSED void (*fn)(void)) {}
  #endif
#endif

static void fs_tmp_init(void) {
  mtx_init(&TMP_INFO_LOCK, mtx_plain);

  MTX_LOCK(&TMP_INFO_LOCK, {
    const char *tmp_dir = getenv("TMPDIR");
    if (!tmp_dir || !tmp_dir[0]) {
      tmp_dir = "/tmp/";
    }

    size_t len = strlen(tmp_dir);
    if (tmp_dir[len - 1] != '/') {
      char *new = malloc((len + 2) * sizeof(*tmp_dir));
      strcpy(new, tmp_dir);

      new[len] = '/';
      new[len + 1] = '\0';

      tmp_dir = new;
    }

    TMP_INFO.tmp_names = vector_create(sizeof(char *));
    TMP_INFO.tmp_dir = MK_USTR(tmp_dir);

    atexit(fs_tmp_del);
    at_quick_exit2(fs_tmp_del);
  });
}

#include <Availability.h>

static void fs_init(void) {
  call_once(&TMP_INFO_FLAG, fs_tmp_init);
}

void fs_create(struct arena_allocator *arena, enum fs_flags flags,
                   struct fs **fs) {
  fs_init();

  *fs = arena_alloc(arena, sizeof(**fs));

  **fs = (struct fs){
      .arena = arena,
      .flags = flags,
      .cache = hashtbl_create_in_arena(arena, sizeof(struct fs_key),
                                       sizeof(struct fs_file),
                                       hash_fs_key, eq_fs_key)};
}

FILE *fs_tmpfile(const char **path) {
  fs_init();

  ustr_t dir;
  MTX_LOCK(&TMP_INFO_LOCK, {
    dir = TMP_INFO.tmp_dir;
  });

  const char *suffix = "jccobjXXXXXXXX.o";
  char *name = nonnull_malloc(dir.len + strlen(suffix) + 1);

  memcpy(name, dir.str, dir.len);
  name[dir.len] = '\0';

  strcat(name, suffix);

  int fd = mkstemps(name, 2);
  if (fd < 0) {
    BUG("mkstemps call failed (%s)!", strerror(errno));
  }

  FILE *file = fdopen(fd, "wb");
  if (!file) {
    BUG("fdopen call failed (%s)!", strerror(errno));
  }

  MTX_LOCK(&TMP_INFO_LOCK, {
    vector_push_back(TMP_INFO.tmp_names, &name);
  });

  if (path) {
    *path = name;
  }

  return file;
}

enum fc_mode {
  FC_MODE_READ,
  FC_MODE_TEST,
};

static bool fs_read(struct fs *fs, struct fs_key key,
                        struct fs_file *file, enum fc_mode mode);

bool fs_test_path(struct fs *fs, ustr_t path) {
  struct fs_key key = {F_TY_PATH, .key = path};

  return fs_read(fs, key, NULL, FC_MODE_TEST);
}

bool fs_read_stdin(struct fs *fs, struct fs_file *file) {
  struct fs_key key = {.ty = F_TY_STDIN, .key = MK_NULL_USTR()};

  return fs_read(fs, key, file, FC_MODE_READ);
}

bool fs_read_path(struct fs *fs, ustr_t path,
                      struct fs_file *file) {
  struct fs_key key = {F_TY_PATH, .key = path};

  return fs_read(fs, key, file, FC_MODE_READ);
}

bool fs_read_proc(struct fs *fs, ustr_t proc,
                      struct fs_file *file) {
  struct fs_key key = {F_TY_PROC, .key = proc};

  return fs_read(fs, key, file, FC_MODE_READ);
}

static void read_file_content(struct fs *fs, FILE *file, char **buf,
                              size_t *buf_len);

static bool fs_read(struct fs *fs, struct fs_key key,
                        struct fs_file *file, enum fc_mode mode) {
  // TODO: caching (via mtime) even when flag not present
  if (fs->flags & FS_FLAG_ASSUME_CONSTANT) {
    struct fs_file *cache = hashtbl_lookup(fs->cache, &key);
    if (cache) {
      if (file) {
        *file = *cache;
      }
      return true;
    }
  }

  // FIXME: assert the sized_str has no null chars in it
  DEBUG_ASSERT(ustr_nullsafe(key.key), "fs doesn't support null chars");

  PROFILE_CREATE_MULTI(fs_read);
  PROFILE_BEGIN_MULTI(fs_read);

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
    PROFILE_END_MULTI(fs_read);
    return false;
  }

  if (file) {
    *file = (struct fs_file){
        .name = key.key,
    };
  }

  if (mode == FC_MODE_READ) {
    DEBUG_ASSERT(file, "file null");

    char *data;
    read_file_content(fs, f, &data, &file->len);

    file->data = data;

    // clone becaue fs may outlive callers
    key.key = arena_alloc_ustrdup(fs->arena, key.key);
    file->name = arena_alloc_ustrdup(fs->arena, file->name);

    hashtbl_insert(fs->cache, &key, file);
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

  PROFILE_END_MULTI(fs_read);
  return true;
}

static void read_file_content(struct fs *fs, FILE *file, char **buf,
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
      content = arena_realloc(fs->arena, content, len + READ_BUF_SZ);
      head = &content[len];

      len += read;

    } while ((read = fread(head, 1, READ_BUF_SZ, file)) > 0);

    content[len] = '\0';

    *buf = content;
    *buf_len = len;
    return;
  }

  fseek(file, 0, SEEK_END);
  fseek(file, 0, SEEK_SET);

  char *content = arena_alloc(fs->arena, (unsigned long)fsize + 1);
  size_t read = fread(content, 1, (unsigned long)fsize, file);

  if (read != (size_t)fsize) {
    *buf = NULL;
    *buf_len = 0;
  }

  content[fsize] = '\0';
  *buf = content;
  *buf_len = (size_t)(fsize + 1);
}
