#ifndef INCLUDE_H
#define INCLUDE_H

#define NULL ((void *)0)

typedef struct FILE FILE;

#ifdef __LP64__
typedef unsigned long long size_t;
typedef long long ssize_t;
#else
typedef unsigned size_t;
typedef long ssize_t;
#endif


#ifdef __APPLE__
extern FILE *__stderrp;
#define stderr __stderrp

extern FILE *__stdoutp;
#define stdout __stdoutp
#elif defined(__riscv__) || defined(__riscv)
extern void **_impure_ptr;
#define stderr ((FILE *)_impure_ptr[3])
extern void **_impure_ptr;
#define stdout ((FILE *)_impure_ptr[2])
#else
extern FILE *stderr;
#endif


#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define	EOF	(-1)


int fprintf(FILE *, const char *, ...);
FILE *fopen(const char *, const char *);
int fseek(FILE *, long, int);
long ftell(FILE *);
void rewind(FILE *);
size_t fwrite(const void *, size_t, size_t, FILE *);
size_t fread(void *, size_t, size_t, FILE *);
int fclose(FILE *);

void clearerr(FILE *);
int	fclose(FILE *);
int	feof(FILE *);
int	ferror(FILE *);
int	fflush(FILE *);
int	fgetc(FILE *);
int getc(FILE *);
char *fgets(char *, int, FILE *);
int	fputc(int, FILE *);
int	fputs(const char *, FILE *);

int printf(const char *, ...);

int printf(const char *, ...);
int sprintf(char *, const char *, ...);

int putchar(int);
int getchar(void);

double sin(double);
double cos(double);
double tan(double);
float sqrtf(float);
float fabsf(float);

double sqrt(double);
double fabs(double);

double fmin(double, double);
double fmax(double, double);
double pow(double, double);

#define INFINITY 1E+310

int rand(void);

#endif

