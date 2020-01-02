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

void initialize_obarray(void);

typedef struct Object Object;

Object cons(Object carCell, Object cdrCell);
Object car(Object consCell);
Object cdr(Object consCell);
Object set_car(Object consCell, Object newCar);
Object set_cdr(Object consCell, Object newCdr);
bool isNumber(Object cell);
bool isSymbol(Object cell);
bool isPair(Object cell);
bool isNull(Object cell);
bool eq(Object o1, Object o2);
bool isSelfEvaluating(Object cell);
Object plus(Object args);

void interpret(void);
Object read(void);

#endif  /* D_FakeReader_H */
