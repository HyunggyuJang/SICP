#include "../src/sicp.h"
#include "../src/type.h"
#include "../../include/dbg.h"
Object entry()
{
val = make_compiled_procedure(make_label(&&entry65), env);
goto after_lambda64;
entry65:
env = compiled_procedure_env(proc);
unev = nil;
save(unev);
unev = intern("n");
unev = cons(unev, restore());
env = extend_environment(unev, argl, env);
save(cont);
save(env);
proc = lookup_variable_value(intern("="), env);
val = (Object) {.type = OB_EXACT, .data = 1};
argl = cons(val, nil);
val = lookup_variable_value(intern("n"), env);
argl = cons(val, argl);
if (proc.type == OB_PRIMITVE) goto primitive_branch80;
compiled_branch79:
cont = make_label(&&after_call78);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch80:
val = apply_primitive_procedure(proc, argl);
after_call78:
env = restore();
cont = restore();
if (!true_p(val)) goto false_branch67;
true_branch68:
val = (Object) {.type = OB_EXACT, .data = 1};
goto *label(cont);
false_branch67:
proc = lookup_variable_value(intern("*"), env);
save(cont);
save(proc);
val = lookup_variable_value(intern("n"), env);
argl = cons(val, nil);
save(argl);
proc = lookup_variable_value(intern("factorial"), env);
save(proc);
proc = lookup_variable_value(intern("-"), env);
val = (Object) {.type = OB_EXACT, .data = 1};
argl = cons(val, nil);
val = lookup_variable_value(intern("n"), env);
argl = cons(val, argl);
if (proc.type == OB_PRIMITVE) goto primitive_branch71;
compiled_branch70:
cont = make_label(&&after_call69);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch71:
val = apply_primitive_procedure(proc, argl);
after_call69:
argl = cons(val, nil);
proc = restore();
if (proc.type == OB_PRIMITVE) goto primitive_branch74;
compiled_branch73:
cont = make_label(&&after_call72);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch74:
val = apply_primitive_procedure(proc, argl);
after_call72:
argl = restore();
argl = cons(val, argl);
proc = restore();
cont = restore();
if (proc.type == OB_PRIMITVE) goto primitive_branch77;
compiled_branch76:
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch77:
val = apply_primitive_procedure(proc, argl);
goto *label(cont);
after_call75:
after_if66:
after_lambda64:
val = define_variable(intern("factorial"), val, env);
proc = lookup_variable_value(intern("factorial"), env);
val = (Object) {.type = OB_EXACT, .data = 10};
argl = cons(val, nil);
if (proc.type == OB_PRIMITVE) goto primitive_branch63;
compiled_branch62:
cont = make_label(&&after_call61);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch63:
val = apply_primitive_procedure(proc, argl);
after_call61:
return val;
}
int main()
{
check_mem(heap_Create(30000));
setup_obarray();
setup_environment();
initialize_stack();
env = global_env;
user_print(entry());
destroy_obarray();
heap_Destroy();
return 0;
error:
heap_Destroy();
return 1;
}
