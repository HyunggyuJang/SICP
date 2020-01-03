#include <Reader.h>
#include <dbg.h>

int main()
{
    check(heap_Create(10000), "Memory error.");
    initialize_obarray();
    setup_environment();
    initialize_stack();
    repl();
    heap_Destory();
    return 0;
error:
    heap_Destory();
    return 1;
}
