#include <formatOut.h>
#include <stdio.h>
#include <stdarg.h>

int inline formatOut(FILE *restrict stream, const char *restrict fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    return vfprintf(stream, fmt, args);
}
