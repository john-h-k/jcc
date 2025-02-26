
typedef struct FILE FILE;

#ifdef __APPLE__
extern FILE *__stderrp;
#define stderr __stderrp
#elifdef __riscv__
extern void **_impure_ptr;
#define stderr ((FILE *)_impure_ptr[3])
#else
extern FILE *stderr;
#endif

int fprintf(FILE *, const char *, ...);

int g(int x);

int main()
{
    int x;
    for(int i=0; i<4; i++){
        fprintf(stderr, "g(%d)==%d\n", i, g(i));
    }
    return !( (g(0)+g(1)+g(2)+g(3))==9);
}
