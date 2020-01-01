#ifndef D_Reader_H
#define D_Reader_H

/**********************************************************
 *
 * Reader is responsible for ...
 *
 **********************************************************/

#include <stdbool.h>

int heap_Create(unsigned long byte_size);
void heap_Destory(void);

typedef struct Object Object;

Object cons(Object carCell, Object cdrCell);
Object car(Object consCell);
Object cdr(Object consCell);
bool isPair(Object cell);

Object read(void);

#endif  /* D_FakeReader_H */
