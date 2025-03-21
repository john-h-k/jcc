// this file contains BMP chars encoded in UTF-8
#include <stdio.h>
// #include <wchar.h>

int main()
{
    int s[] = L"hello$$你好¢¢世界€€world";
    int *p;
    for (p = s; *p; p++) printf("%04X ", (unsigned) *p);
    printf("\n");
    return 0;
}
