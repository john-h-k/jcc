
typedef struct FILE FILE;

extern FILE *__stderrp;
#define stderr __stderrp

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

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
