#ifndef __SICP_H_
#define __SICP_H_
#include <stdbool.h>

typedef struct Object Object;

int heap_Create(unsigned long byte_size);
void heap_Destroy(void);
void setup_obarray(void);
void destroy_obarray(void);
void initialize_stack(void);
void setup_environment(void);
void user_print(Object val);

/* SCode */
Object intern(char *str);
Object extend_environment(Object vars, Object vals, Object base_env);
Object lookup_variable_value(Object var, Object env);
Object set_variable_value(Object var, Object val, Object env);
Object define_variable(Object var, Object val, Object env);
Object make_string_obj(const char *cstring);
Object make_double_obj(double data);
Object make_compiled_procedure(Object entry, Object env);
Object compiled_procedure_entry(Object proc);
Object compiled_procedure_env(Object proc);
void *label(Object lab_obj);
Object make_label(void *label);
Object apply_primitive_procedure(Object proc, Object argl);

bool true_p(Object exp);

extern Object expr, val, unev, env, argl, proc, cont, nil, global_env;

int save(Object reg);
Object restore(void);
#endif // __SICP_H_
