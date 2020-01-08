#ifndef __READER_INTERNAL_H_
#define __READER_INTERNAL_H_

/* #define GC_WORK */
/* To make work GC_WORK, we need to underlying C's implicit control system to cope with
   following situations
Object Prim_cons(Object args)
{
    return cons(car(args), car(cdr(args)));
}
Object add_binding_to_frame(Object var, Object val, Object frame)
{
    set_car(frame, cons(var, car(frame)));
    return set_cdr(frame, cons(val, cdr(frame)));
}
...
cons(quote, cons(returnValue, nil));
where the parameters of cons got bound by C's pass by value implicit control system.
   That is, we need to convert C language into explicit control language -- machine language
   -- as we did in SICP.
 */
#define GC_WORK

typedef struct Object {
  unsigned char type;
  unsigned long len : 56;
  unsigned long data;
} Object;


#define LEN_MASK 0x00ffffffffffffff
Object make_string_obj(const char *cstring);
char *getString(Object cell);

extern Object nil;
extern Object err;
extern Object ok;

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
  OB_COMPOUND,
  OB_LABEL,
  OB_BOOLEAN
};

typedef enum JUMP_LIST
{
done,
ev_sequence_continue,
ev_appl_did_operator,
ev_appl_accumulate_arg,
ev_appl_accum_last_arg,
ev_assignment_1,
ev_if_decide,
ev_definition_1
} JUMP_LIST;

extern Object *heap;

/* obarray */
Object intern(char *str);

/* environment */
extern Object the_empty_env;
extern Object global_env;
Object make_frame(Object variables, Object values);
Object extend_frame(Object vars, Object vals, Object base_env);
Object lookup_variable_value(Object var, Object env);
Object text_of_quotation(Object exp);
bool quoted_p(Object exp);

Object cadadr(Object exp);
Object cadr(Object exp);
Object cddr(Object exp);
Object caddr(Object exp);
Object cdddr(Object exp);
Object cadddr(Object exp);
Object caadr(Object exp);
Object cdadr(Object exp);
Object cadadr(Object exp);

int save(Object reg);
Object restore(void);
extern int max_depth;

Object apply_primitive_procedure(Object proc, Object argl);

/* Garbage collection */
Object relocate_old_result_in_new(Object old);
#if defined (GC_ALTERNATIVE) || defined (GC_WORK)
int gc(void);
extern Object *Freep;
extern Object *tospace;
Object registerSymbolObject(Object symObj);
#endif

#ifdef GC_WORK
int push(Object *object);
Object *pop(void);
#endif

extern char token[];
enum TokenType
{
ERR = -8, EOL, WORD,
EXACT, INEXACT, STRING
};

int getToken(void);

#endif // __READER_INTERNAL_H_
