#ifndef __READER_INTERNAL_H_
#define __READER_INTERNAL_H_

#define TOLONG

typedef struct Object {
    unsigned char type;
    unsigned long len : 56;
#ifdef TOLONG
  unsigned long data;
#else
    double data;
#endif
} Object;

#define LEN_MASK 0x00ffffffffffffff
Object make_string_obj(const char *cstring);
char *getString(Object cell);

extern Object nil;
extern Object err;
extern Object ok;

enum OBJECT_TYPES
{ OB_ERR,
  OB_NIL,
  OB_PAIR,
  OB_EXACT,
  OB_INEXACT,
  OB_STRING,
  OB_SYMBOL,
  OB_PRIMITVE
};

enum JUMP_LIST
{
READ_EVAL_PRINT_LOOP = 0,
PRINT_RESULT,
};

extern Object *heap;

/* obarray */
Object intern(Object str);

/* environment */
extern Object the_empty_env;
Object make_frame(Object variables, Object values);
Object extend_frame(Object vars, Object vals, Object base_env);
Object lookup_variable_value(Object var, Object env);

extern char token[];
enum TokenType
{
ERR = -8, EOL, WORD,
EXACT, INEXACT, STRING
};

int getToken(void);

#endif // __READER_INTERNAL_H_
