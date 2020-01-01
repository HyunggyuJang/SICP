#ifndef __READER_INTERNAL_H_
#define __READER_INTERNAL_H_


struct Object {
    unsigned char type;
    char string[7];
    double data;
};

enum OBJECT_TYPES
{ OB_ERR, OB_NIL, OB_PAIR, OB_EXACT, OB_INEXACT, OB_STRING };

extern struct Object *heap;

extern char token[];
enum TokenType
{
ERR = -8, EOL, WORD,
EXACT, INEXACT, STRING
};

int getToken(void);

#endif // __READER_INTERNAL_H_
