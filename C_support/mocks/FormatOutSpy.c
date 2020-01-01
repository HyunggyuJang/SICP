#include "FormatOutSpy.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <dbg.h>

typedef struct OutChars {
    char *start;
    size_t offset;
    size_t end;
} OutChars;

static OutChars *stdoutBuf = NULL;
static OutChars *stderrBuf = NULL;

int FormatOutSpy_Create(size_t bufMax)
{
    stdoutBuf = malloc(sizeof (OutChars));
    check_mem(stdoutBuf);
    stderrBuf = malloc(sizeof (OutChars));
    check_mem(stderrBuf);

    stdoutBuf->start = malloc(bufMax + 1); // for last null character
    check_mem(stdoutBuf->start);
    stdoutBuf->start[0] = '\0';
    stdoutBuf->end = bufMax;
    stdoutBuf->offset = 0;

    stderrBuf->start = malloc(bufMax + 1);  // same as above
    check_mem(stderrBuf->start);
    stderrBuf->start[0] = '\0';
    stderrBuf->end = bufMax;
    stderrBuf->offset = 0;

    return 1;

error:
    if (stdoutBuf) {
        if (stdoutBuf->start)
            free(stdoutBuf->start);
        free(stdoutBuf);
    }
    if (stderrBuf) {
        if (stderrBuf->start)
            free(stderrBuf->start);
        free(stderrBuf);
    }
    return 0;
}

int formatOut(FILE *restrict stream, const char *restrict fmt, ...)
{
    OutChars *outbuf = NULL;
    if (stream == stdout) {
        outbuf = stdoutBuf;
    } else if (stream == stderr) {
        outbuf = stderrBuf;
    } else {
        sentinel("FormatOutSpy only handle stdio or stderr.");
    }

    va_list args;
    va_start(args, fmt);

    int availableSpace = outbuf->end - outbuf->offset + 1; // null character
    int virtual_written = vsnprintf(&outbuf->start[outbuf->offset],
                            availableSpace,
                            fmt, args);

    int actual_written = // except null character
        virtual_written > availableSpace - 1 ? availableSpace - 1 : virtual_written;
    outbuf->offset += actual_written;

    return actual_written;

error:
    return EOF;
}

char *FormatOutSpy_GetOutput(FILE *restrict stream)
{
    OutChars *outbuf = NULL;
    if (stream == stdout) {
        outbuf = stdoutBuf;
    } else if (stream == stderr) {
        outbuf = stderrBuf;
    } else {
        sentinel("FormatOutSpy only handle stdio or stderr.");
    }

    return outbuf->start;

error:
    return NULL;
}

void FormatOutSpy_Destroy(void)
{
    if (stdoutBuf)
    {
        if (stdoutBuf->start)
            free(stdoutBuf->start);
        free(stdoutBuf);
    }
    if (stderrBuf)
    {
        if (stderrBuf->start)
            free(stderrBuf->start);
        free(stderrBuf);
    }
}
