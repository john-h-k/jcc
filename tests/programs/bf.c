#define NULL ((void *)0)
#define MAX_CELL_SIZE 30000

struct State {
  // int cells[MAX_CELL_SIZE];
  int *cells;
  int pos;
};

typedef unsigned long size_t;

typedef struct FILE FILE;
extern FILE *__stderrp;
#define stderr __stderrp

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

int fprintf(FILE *, const char *, ...);
FILE *fopen(const char *, const char *);
int fseek(FILE *, long, int);
long ftell(FILE *);
void rewind(FILE *);
size_t fread(void *, size_t, size_t, FILE *);
int fclose(FILE *);

int printf(const char *, ...);

int putchar(int);
int getchar();

void *memset(void *str, int c, unsigned long n);
void *malloc(size_t);
void free(void *);

void exit(int);

void error(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

int read_cur_cell(struct State *state) { return state->cells[state->pos]; }

void write_cur_cell(struct State *state, int value) {
  state->cells[state->pos] = value;
}

void inc_cur_cell(struct State *state) {
  write_cur_cell(state, read_cur_cell(state) + 1);
}

void dec_cur_cell(struct State *state) {
  write_cur_cell(state, read_cur_cell(state) - 1);
}

int main(int argc, char **argv) {
  if (argc != 2) {
    error("You must provide a filename as an argument");
  }

  FILE *file = fopen(argv[1], "r");
  if (file == NULL) {
    error("Could not open file");
  }

  fseek(file, 0, SEEK_END);
  long filesize = ftell(file);
  rewind(file);

  char *program = malloc(filesize + 1);
  if (program == NULL) {
    error("Not enough memory to read file");
  }

  fread(program, 1, filesize, file);
  program[filesize] = '\0'; // Null-terminate the buffer

  // struct State state = { .pos = 0 };
  struct State state;
  state.pos = 0;
  state.cells = malloc(sizeof(*state.cells) * MAX_CELL_SIZE);
  memset(state.cells, 0, sizeof(*state.cells) * MAX_CELL_SIZE);
  for (int i = 0; i < MAX_CELL_SIZE; i++)
    state.cells[i] = 0;

  char command;
  int instr_pointer = 0;

  while (instr_pointer < filesize) {
    command = program[instr_pointer];

    switch (command) {
    case '+':
      inc_cur_cell(&state);
      break;
    case '-':
      dec_cur_cell(&state);
      break;
    case '>':
      state.pos++;
      break;
    case '<':
      if (state.pos == 0) {
        error("Attempted to decrement data pointer below 0");
      }
      state.pos--;
      break;
    case '.':
      putchar(read_cur_cell(&state));
      break;
    case ',':
      write_cur_cell(&state, getchar());
      break;
    case '[':
      if (read_cur_cell(&state) == 0) {
        int depth = 1;
        while (depth > 0 && instr_pointer < filesize) {
          instr_pointer++;
          if (program[instr_pointer] == '[') {
            depth++;
          } else if (program[instr_pointer] == ']') {
            depth--;
          }
        }
      }
      break;
    case ']':
      if (read_cur_cell(&state) != 0) {
        int depth = 1;
        while (depth > 0 && instr_pointer >= 0) {
          instr_pointer--;
          if (program[instr_pointer] == ']') {
            depth++;
          } else if (program[instr_pointer] == '[') {
            depth--;
          }
        }
      }
      break;
    }
    instr_pointer++;
  }

  free(program);
  fclose(file);

  return 0;
}
