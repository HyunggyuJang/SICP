#include <getChar.h>
#include <stdio.h>

inline int getChar(void)
{
    return getc(stdin);
}

inline int unGetc(int pushedBack)
{
    return ungetc(pushedBack, stdin);
}
