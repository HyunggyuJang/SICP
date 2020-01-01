#include "GetCharSpy.h"
#include <stdio.h>

static const char *sourceChars = NULL;
static int pusedBackChar = EOF;
void GetCharSpy_Create(const char *inputStream)
{
    sourceChars = inputStream;
    pusedBackChar = EOF;
}

void GetCharSpy_Destroy(void)
{
}

int getChar(void)
{
    if (pusedBackChar != EOF) {
        int returnC = pusedBackChar;
        pusedBackChar = EOF;
        return returnC;
    }
    if (!sourceChars || !*sourceChars)
        return EOF;
    return *sourceChars++;
}

int unGetc(int pushedBack)
{
    return pusedBackChar = pushedBack;
}
