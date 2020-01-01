#ifndef D_FormatOutSpy_H
#define D_FormatOutSpy_H

/**********************************************************
 *
 * FormatOutSpy is responsible for ...
 *
 **********************************************************/
#include <stddef.h>
#include <formatOut.h>

int FormatOutSpy_Create(size_t bufMax);
void FormatOutSpy_Destroy(void);
char *FormatOutSpy_GetOutput(FILE *stream);

#endif  /* D_FakeFormatOutSpy_H */
