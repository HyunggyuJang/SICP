#include "../src/sicp.h"
#include "../src/type.h"
#include "../../include/dbg.h"
Object entry()
{
val = make_compiled_procedure(make_label(&&entry85), env);
goto after_lambda84;
entry85:
env = compiled_procedure_env(proc);
unev = nil;
save(unev);
unev = intern("n");
unev = cons(unev, restore());
env = extend_environment(unev, argl, env);
save(cont);
save(env);
proc = lookup_variable_value(intern("<"), env);
val = (Object) {.type = OB_EXACT, .data = 2};
argl = cons(val, nil);
val = lookup_variable_value(intern("n"), env);
argl = cons(val, argl);
if (proc.type == OB_PRIMITVE) goto primitive_branch106;
compiled_branch105:
cont = make_label(&&after_call104);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch106:
val = apply_primitive_procedure(proc, argl);
after_call104:
env = restore();
cont = restore();
if (!true_p(val)) goto false_branch87;
true_branch88:
val = lookup_variable_value(intern("n"), env);
goto *label(cont);
false_branch87:
proc = lookup_variable_value(intern("+"), env);
save(cont);
save(proc);
save(env);
proc = lookup_variable_value(intern("fib"), env);
save(proc);
proc = lookup_variable_value(intern("-"), env);
val = (Object) {.type = OB_EXACT, .data = 2};
argl = cons(val, nil);
val = lookup_variable_value(intern("n"), env);
argl = cons(val, argl);
if (proc.type == OB_PRIMITVE) goto primitive_branch97;
compiled_branch96:
cont = make_label(&&after_call95);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch97:
val = apply_primitive_procedure(proc, argl);
after_call95:
argl = cons(val, nil);
proc = restore();
if (proc.type == OB_PRIMITVE) goto primitive_branch100;
compiled_branch99:
cont = make_label(&&after_call98);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch100:
val = apply_primitive_procedure(proc, argl);
after_call98:
argl = cons(val, nil);
env = restore();
save(argl);
proc = lookup_variable_value(intern("fib"), env);
save(proc);
proc = lookup_variable_value(intern("-"), env);
val = (Object) {.type = OB_EXACT, .data = 1};
argl = cons(val, nil);
val = lookup_variable_value(intern("n"), env);
argl = cons(val, argl);
if (proc.type == OB_PRIMITVE) goto primitive_branch91;
compiled_branch90:
cont = make_label(&&after_call89);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch91:
val = apply_primitive_procedure(proc, argl);
after_call89:
argl = cons(val, nil);
proc = restore();
if (proc.type == OB_PRIMITVE) goto primitive_branch94;
compiled_branch93:
cont = make_label(&&after_call92);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch94:
val = apply_primitive_procedure(proc, argl);
after_call92:
argl = restore();
argl = cons(val, argl);
proc = restore();
cont = restore();
if (proc.type == OB_PRIMITVE) goto primitive_branch103;
compiled_branch102:
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch103:
val = apply_primitive_procedure(proc, argl);
goto *label(cont);
after_call101:
after_if86:
after_lambda84:
val = define_variable(intern("fib"), val, env);
proc = lookup_variable_value(intern("fib"), env);
val = (Object) {.type = OB_EXACT, .data = 30};
argl = cons(val, nil);
if (proc.type == OB_PRIMITVE) goto primitive_branch83;
compiled_branch82:
cont = make_label(&&after_call81);
val = compiled_procedure_entry(proc);
goto *label(val);
primitive_branch83:
val = apply_primitive_procedure(proc, argl);
after_call81:
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
