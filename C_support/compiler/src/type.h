#ifndef __TYPE_H_
#define __TYPE_H_

#include <stdbool.h>

typedef struct Object {
  unsigned char type;
  unsigned long len : 56;
  unsigned long data;
} Object;

#define LEN_MASK 0x00ffffffffffffff
typedef Object (*primproc_t)(Object args);

enum OBJECT_TYPES
{ OB_ERR,
  OB_BROKEN_HEART,
  OB_STRING_DATA,
  OB_NIL,
  OB_PAIR,
  OB_EXACT,
  OB_INEXACT,
  OB_STRING,
  OB_SYMBOL,
  OB_PRIMITVE,
  OB_COMPILED,
  OB_LABEL,
  OB_BOOLEAN
};

char *getString(Object cell);

int push(Object *object);
Object *pop(void);

int gc(void);

Object car(Object consCell);
Object cdr(Object consCell);
Object cons(Object carCell, Object cdrCell);
extern Object err, ok;
bool inline eq(Object o1, Object o2);
Object registerSymbolObject(Object symObj);

#endif // __TYPE_H_
