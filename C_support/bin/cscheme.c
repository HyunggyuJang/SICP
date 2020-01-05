#include <Reader.h>
#include <dbg.h>

int main()
{
    check(heap_Create(30000), "Memory error.");
    setup_obarray();
    setup_environment();
    initialize_stack();
    repl();
    destroy_obarray();
    heap_Destory();
    return 0;
error:
    heap_Destory();
    return 1;
}
