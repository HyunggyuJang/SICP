#include "sicp.h"
#include "type.h"
#include <stdlib.h>
#include <dbg.h>

/* Memory allocation */
Object *heap = NULL;
Object *Freep = NULL;
unsigned long word_size = 0;

int heap_Create(unsigned long byte_size)
{
    word_size = (sizeof (Object) - 1 + byte_size) / sizeof (Object);
    heap = malloc(word_size * sizeof (Object));
    Freep = heap;
    check_mem(heap);
    return 1;

error:
    word_size = 0;
    return 0;
}

void heap_Destroy(void)
{
    if (heap)
        free(heap);
}

/* scheme types */
bool inline isNull(Object cell)
{
    return cell.type == OB_NIL;
}

Object cons(Object carCell, Object cdrCell)
{
    push(&cdrCell);
    push(&carCell);
    if (!(Freep - heap + 2 < word_size)) {
        debug("Call garbage collector. -- CONS");
        check(gc(), "Failed to clean up garbage."); // in addition to these, we need to update the contents of stack
    }
    check(Freep - heap + 2 < word_size, "Heap overflow. -- CONS");
    Object consCell;
    consCell.type = OB_PAIR;
    consCell.data = (unsigned long) Freep;
    *Freep++ = carCell;
    *Freep++ = cdrCell;
    pop();
    pop();
    return consCell;
error:
    return err;
}

Object Prim_cons(Object args)
{
    return cons(car(args), car(cdr(args)));
}

Object car(Object consCell)
{
    check(!eq(consCell, err), "Invalid consCell -- CAR.");
    return *((Object *) consCell.data);
error:
    return err;
}

Object cdr(Object consCell)
{
    check(!eq(consCell, err), "Invalid consCell -- CDR.");
    return *((Object *) consCell.data + 1);
error:
    return err;
}

Object cadr(Object exp)
{
    return car(cdr(exp));
}

Object set_car(Object consCell, Object newCar)
{
    *((Object *) consCell.data) = newCar;
    return ok;
}

Object Prim_set_car(Object args)
{
    return set_car(car(args), cadr(args));
}

Object set_cdr(Object consCell, Object newCdr)
{
    *((Object *) consCell.data + 1) = newCdr;
    return ok;
}

Object Prim_set_cdr(Object args)
{
    return set_cdr(car(args), cadr(args));
}

char inline *getString(Object cell)
{
    return (char *) &((Object *)cell.data)->data;
}

bool inline isNumber(Object cell)
{
    return cell.type == OB_INEXACT || cell.type == OB_EXACT;
}

bool inline isSymbol(Object cell)
{
    return cell.type == OB_SYMBOL;
}

bool inline eq(Object o1, Object o2)
{
    return o1.type == o2.type && o1.data == o2.data;
}

// end of memory allocation

// constants
Object nil = {.type = OB_NIL};
Object err = {.type = OB_ERR};
Object sharp_t = {.type = OB_BOOLEAN, .data = (unsigned long) true};
Object sharp_f = {.type = OB_BOOLEAN, .data = (unsigned long) false};

// symbols
Object ok;
Object quote;
Object define;
Object if_s;
Object lambda;
Object begin;
Object set_bang;
Object true_s;
Object false_s;

typedef struct Obnode {
    Object symbol;
    struct Obnode *next;
} Obnode;

Obnode obHeader = {{.type = OB_NIL}, NULL};

#define DEFAULT_OBARRY_SIZE 101
#define OBARRAY_SIZE (sizeof obarray / sizeof obarray[0])

static Obnode obarray[DEFAULT_OBARRY_SIZE];

void initialize_obarray(void)
{
    for (int i = 0; i < OBARRAY_SIZE; i++)
        obarray[i] = obHeader;
}

void set_default_symbols(void)
{
    ok = intern("ok");
    quote = intern("quote");
    define = intern("define");
    if_s = intern("if");
    lambda = intern("lambda");
    begin = intern("begin");
    set_bang = intern("set!");
    true_s = intern("true");
    false_s = intern("false");
}

void setup_obarray(void)
{
    initialize_obarray();
    set_default_symbols();
}

static unsigned hash(char *s)
{
    unsigned hashval;

    for (hashval = 0; *s != '\0'; s++)
        hashval = *s + 31 * hashval;
    return hashval % OBARRAY_SIZE;
}

void destroy_obnode(Obnode *node)
{
    Obnode *bucket = NULL;
    Obnode *prev = NULL;
    if (!node->next) // header
        return;
    for (prev = node->next, bucket = prev->next; bucket; prev = bucket, bucket = prev->next)
        free(prev);
    free(prev);
}

void destroy_obarray(void)
{
    for (int i = 0; i < OBARRAY_SIZE; i++)
        destroy_obnode(&obarray[i]);
}

Object make_string_obj_gc(const char *cstring)
{
    unsigned long cstrlen = strlen(cstring);
    check(cstrlen <= LEN_MASK, "given string is too long.");

    Object str = {.type = OB_STRING, .len = cstrlen};
    size_t sizeInWords = // account null character + type specifier for gc + sizeInWords
        (sizeof (Object) + str.len + sizeof (unsigned long)) / sizeof (Object);
    check_debug(Freep - heap + sizeInWords < word_size,
          "Need to call gc() -- MAKE_STRING_OBJ_GC");
    Freep->type = OB_STRING_DATA;
    Freep->len = (unsigned long) sizeInWords;
    str.data = (unsigned long)Freep;
    memcpy((char *)&Freep->data, cstring, str.len + 1);
    Freep += sizeInWords;
    return str;
error:
    return err;
}

Object intern(char *str)
{
    unsigned hashval = hash(str);
    Obnode *bucket = NULL;
    Obnode *prev = NULL;
    int cmpResult = 0;
    for (prev = &obarray[hashval], bucket = prev->next; bucket; prev = bucket, bucket = prev->next) {
        cmpResult = strcmp(str, getString(bucket->symbol));
        if (cmpResult == 0)
            return bucket->symbol;
        if (cmpResult < 0) {
            prev->next = malloc(sizeof (Obnode));
            prev->next->symbol = make_string_obj_gc(str);
            check_debug(!eq(prev->next->symbol, err), "Go to call gc. -- INTERN");
            prev->next->symbol.type = OB_SYMBOL;
            prev->next->next = bucket;
            return prev->next->symbol;
        }
    }
    prev->next = malloc(sizeof (Obnode));
    prev->next->symbol = make_string_obj_gc(str);
    check_debug(!eq(prev->next->symbol, err), "Go to call gc. -- INTERN");
    prev->next->symbol.type = OB_SYMBOL;
    prev->next->next = bucket;
    return prev->next->symbol;
error:
    gc();
    Object stringObj = make_string_obj_gc(str);
    if (eq(stringObj, err)) {
        log_err("Heap is full -- INTERN");
        return err;
    }
    stringObj.type = OB_SYMBOL;
    return registerSymbolObject(stringObj);
}
// end of obarray

// environment
Object the_empty_env = {.type = OB_NIL};
Object global_env = {.type = OB_NIL};

Object make_frame(Object variables, Object values)
{
    return cons(variables, values);
}

Object frame_variables(Object frame)
{
    return car(frame);
}

Object frame_values(Object frame)
{
    return cdr(frame);
}

static long length(Object list)
{
    long len = 0;
    for (len = 0; list.type == OB_PAIR;
         len++, list = cdr(list))
        ;
    return isNull(list) ? len : -(len + 1); // for non-list detection
}

Object add_binding_to_frame(Object var, Object val, Object frame)
{
    push(&frame);
    push(&var);
    push(&val);
    set_car(frame, cons(var, car(frame)));
    Object ret = set_cdr(frame, cons(val, cdr(frame)));
    pop();
    pop();
    pop();
    return ret;
}

Object extend_environment(Object vars, Object vals, Object base_env)
{
    long var_len = length(vars);
    long val_len = length(vals);
    check(var_len == val_len || var_len < 0 && -var_len <= val_len,
          "number of variables and values should match; vars' %ld, vals' %ld",
          var_len, val_len);
    push(&base_env);
    Object ret = cons(make_frame(vars, vals), base_env);
    pop();
    return ret;

error:
    return err;
}

static Object first_frame(Object env)
{
    return car(env);
}

static Object enclosing_frame(Object env)
{
    return cdr(env);
}

Object lookup_variable_value(Object var, Object env)
{
    Object frame, vars, vals;
    for (; !eq(env, the_empty_env); env = enclosing_frame(env)) {
        frame = first_frame(env);
        for (vars = frame_variables(frame), vals = frame_values(frame);
             vars.type == OB_PAIR;
             vars = cdr(vars), vals = cdr(vals))
            if (eq(var, car(vars)))
                return car(vals);
        /* To support dot notation in procedure argument */
        if (!isNull(vars) && eq(var, vars))
            return vals;
    }

    log_err("Unbound variable %s", getString(var));
    return err;
}

Object set_variable_value(Object var, Object val, Object env)
{
    Object frame, vars, vals;
    for (; !eq(env, the_empty_env); env = enclosing_frame(env)) {
        frame = first_frame(env);
        for (vars = frame_variables(frame), vals = frame_values(frame);
             !isNull(vars);
             vars = cdr(vars), vals = cdr(vals))
            if (eq(var, car(vars)))
                return set_car(vals, val);
    }
    log_err("Unbound variable -- SET! %s", getString(var));
    return err;
}

Object define_variable(Object var, Object val, Object env)
{
    Object frame, vars, vals;
    frame = first_frame(env);
    for (vars = frame_variables(frame), vals = frame_values(frame);
         !isNull(vars);
         vars = cdr(vars), vals = cdr(vals))
        if (eq(var, car(vars)))
            return set_car(vals, val);
    return add_binding_to_frame(var, val, frame);
}

Object make_string_obj(const char *cstring)
{
    unsigned long cstrlen = strlen(cstring);
    check(cstrlen <= LEN_MASK, "given string is too long.");

    Object str = {.type = OB_STRING, .len = cstrlen};
    size_t sizeInWords = // account null character + type specifier for gc + sizeInWords
        (sizeof (Object) + str.len + sizeof (unsigned long)) / sizeof (Object);
    if (!(Freep - heap + sizeInWords < word_size)) {
        debug("Call the garbage collector -- MAKE_STRING_OBJ.");
        check(gc(), "Failed to cleanup garbage."); // in addition to these, we need to update the contents of stack
    }
    check(Freep - heap + sizeInWords < word_size,
          "Heap overflow -- MAKE_STRING_OBJ.");
    Freep->type = OB_STRING_DATA;
    Freep->len = (unsigned long) sizeInWords;
    str.data = (unsigned long)Freep;
    memcpy((char *)&Freep->data, cstring, str.len + 1);
    Freep += sizeInWords;
    return str;
error:
    return err;
}

// primitive procedures
Object plus(Object args)
{
    switch (args.type) {
        case OB_NIL:
            args.type = OB_EXACT;
            args.data = 0L;
            return args;
        case OB_EXACT:
        case OB_INEXACT:
            return args;
        case OB_PAIR:
        {
            Object augend = car(args);
            check(isNumber(augend), "+ need number arguments.");
            Object addend = plus(cdr(args));
            if (eq(addend, err))
                goto error;
            args.type =
                augend.type == OB_INEXACT ||
                addend.type == OB_INEXACT ?
                OB_INEXACT :
                OB_EXACT;
            if (args.type == OB_EXACT)
                args.data =
                     augend.data +  addend.data;
            else {
                double temp =
                    (augend.type == OB_EXACT ?
                     (long) augend.data :
                     *(double*)&augend.data)
                    + (addend.type == OB_EXACT ?
                       (long) addend.data :
                       *(double*)&addend.data);
                args.data = *(long *) &temp;
            }
            return args;
        }
        default:
            sentinel("+ need number arguments.");
    }

error:
    return err;
}

Object multiply(Object args)
{
    switch (args.type) {
        case OB_NIL:
            args.type = OB_EXACT;
            args.data = 1L;
            return args;
        case OB_EXACT:
        case OB_INEXACT:
            return args;
        case OB_PAIR:
        {
            Object augend = car(args);
            check(isNumber(augend), "* need number arguments.");
            Object addend = multiply(cdr(args));
            if (eq(addend, err))
                goto error;
            args.type =
                augend.type == OB_INEXACT ||
                addend.type == OB_INEXACT ?
                OB_INEXACT :
                OB_EXACT;
            if (args.type == OB_EXACT)
                args.data =
                     augend.data * addend.data;
            else {
                double temp =
                    (augend.type == OB_EXACT ?
                     (long) augend.data :
                     *(double*)&augend.data)
                    * (addend.type == OB_EXACT ?
                       (long) addend.data :
                       *(double*)&addend.data);
                args.data = *(long *) &temp;
            }
            return args;
        }
        default:
            sentinel("* need number arguments.");
    }

error:
    return err;
}

Object minus(Object args)
{
    check(length(args) == 2, "- is binary.");
    Object left = car(args);
    check(left.type == OB_EXACT || left.type == OB_INEXACT, "-: left argument is not a number.");
    Object right = cadr(args);
    check(right.type == OB_EXACT || right.type == OB_INEXACT, "-: right argument is not a number.");
    return (Object) {
        .type = left.type == OB_EXACT && right.type == OB_EXACT ? OB_EXACT : OB_INEXACT,
            .data = (unsigned long)
            ((left.type == OB_INEXACT ? *(double *)& left.data : (long) left.data)
             - (right.type == OB_INEXACT ? *(double *)& right.data : (long) right.data))
            };
error:
    return err;
}

Object lessThan(Object args)
{
    check(length(args) == 2, "< is binary.");
    Object left = car(args);
    check(left.type == OB_EXACT || left.type == OB_INEXACT, "<: left argument is not a number.");
    Object right = cadr(args);
    check(right.type == OB_EXACT || right.type == OB_INEXACT, "<: right argument is not a number.");
    return (Object) { .type = OB_BOOLEAN,
            .data = (unsigned long)
            ((left.type == OB_INEXACT ? *(double *)& left.data : (long) left.data)
             < (right.type == OB_INEXACT ? *(double *)& right.data : (long) right.data))};
error:
    return err;
}

Object greaterThan(Object args)
{
    check(length(args) == 2, "> is binary.");
    Object left = car(args);
    check(left.type == OB_EXACT || left.type == OB_INEXACT, ">: left argument is not a number.");
    Object right = cadr(args);
    check(right.type == OB_EXACT || right.type == OB_INEXACT, ">: right argument is not a number.");
    return (Object) { .type = OB_BOOLEAN,
            .data = (unsigned long)
            ((left.type == OB_INEXACT ? *(double *)& left.data : (long) left.data)
             > (right.type == OB_INEXACT ? *(double *)& right.data : (long) right.data))};
error:
    return err;
}

Object Prim_eq_p(Object args)
{
    check(length(args) == 2, "eq? is binary.");
    return (Object) {.type = OB_BOOLEAN, .data = (unsigned long) eq(car(args), cadr(args))};
error:
    return err;
}

Object equalTo(Object args)
{
    check(length(args) == 2, "= is binary.");
    Object left = car(args);
    check(left.type == OB_EXACT || left.type == OB_INEXACT, "=: left argument is not a number.");
    Object right = cadr(args);
    check(right.type == OB_EXACT || right.type == OB_INEXACT, "=: right argument is not a number.");
    return (Object) { .type = OB_BOOLEAN,
            .data = (unsigned long)
            ((left.type == OB_INEXACT ? *(double *)& left.data : (long) left.data)
             == (right.type == OB_INEXACT ? *(double *)& right.data : (long) right.data))};
error:
    return err;
}

Object make_primitive_procedure(primproc_t proc)
{
    Object primitive_proc = {.type = OB_PRIMITVE,
                             .data = (unsigned long) proc};
    return primitive_proc;
}

Object apply_primitive_procedure(Object proc, Object argl)
{
    return ((primproc_t) proc.data)(argl);
}

Object make_double_obj(double data)
{
    return (Object) {.type = OB_INEXACT, .data = *(long *) &data};
}

Object stack[300];
Object *sp = stack;
int max_depth = 0;
int current_depth = 0;

int save(Object reg)
{
    check(current_depth < 300, "Stack overflow.");
    stack[current_depth++] = reg;
    if (current_depth > max_depth)
        max_depth = current_depth;
    return 1;
error:
    exit(1);
}

Object restore(void)
{
    check(current_depth > 0, "No element in stack.");
    return stack[--current_depth];
error:
    return err;
}

Object *toUpdateStack[100];
Object **_sp = toUpdateStack;

int push(Object *object)
{
    check(_sp < toUpdateStack + 100, "toUpdateStack overflow.");
    *_sp++ = object;
    return 1;
error:
    exit(1);
}

Object *pop(void)
{
    check(_sp > toUpdateStack, "toUpdateStack has no element in it -- POP");
    return *--_sp;
error:
    return NULL;
}

void initialize_stack(void)
{
    current_depth = max_depth = 0;
}

Object primitive_procedure_names = {.type = OB_NIL};
Object primitive_procedure_objects = {.type = OB_NIL};

void addto_primitive_procedures(char *schemeName, primproc_t proc)
{
    push(&primitive_procedure_names);
    push(&primitive_procedure_objects);
    primitive_procedure_names = cons(intern(schemeName), primitive_procedure_names);
    primitive_procedure_objects = cons(make_primitive_procedure(proc), primitive_procedure_objects);
    pop();
    pop();
}

void setup_primitive_procedures()
{
    primitive_procedure_objects =
        primitive_procedure_names = nil;
    addto_primitive_procedures("car", car);
    addto_primitive_procedures("cdr", cdr);
    addto_primitive_procedures("cons", Prim_cons);
    addto_primitive_procedures("set-car!", Prim_set_car);
    addto_primitive_procedures("set-cdr!", Prim_set_cdr);
    addto_primitive_procedures("eq?", Prim_eq_p);
    addto_primitive_procedures("+", plus);
    addto_primitive_procedures("-", minus);
    addto_primitive_procedures("*", multiply);
    addto_primitive_procedures("<", lessThan);
    addto_primitive_procedures(">", greaterThan);
    addto_primitive_procedures("=", equalTo);
}

void setup_environment()
{
    setup_primitive_procedures();
    global_env =
        extend_environment(primitive_procedure_names,
                     primitive_procedure_objects,
                     the_empty_env);
    define_variable(true_s, sharp_t, global_env);
    define_variable(false_s, sharp_f, global_env);
}

Object expr, val, unev, env, argl, proc;
Object cont = {.type = OB_LABEL, .data = (unsigned long) NULL};

void *label(Object lab_obj)
{
    check(lab_obj.type == OB_LABEL, "label accept only label object.");
    return (void *) lab_obj.data;
error:
    return NULL;
}

Object make_label(void *label)
{
    return (Object) {.type = OB_LABEL, .data = (unsigned long)label};
}

bool inline true_p(Object exp)
{
    return !(exp.type == OB_BOOLEAN) || (bool) exp.data;
}

Object make_compiled_procedure(Object entry, Object env)
{
    push(&env);
    if (!(Freep - heap + 2 < word_size)) {
        debug("Call garbage collector. -- MAKE_COMPILED_PROCEDURE");
        check(gc(), "Failed to clean up garbage."); // in addition to these, we need to update the contents of stack
    }
    check(Freep - heap + 2 < word_size, "Heap overflow. -- MAKE_COMPILED_PROCEDURE");
    Object compiledProc;
    compiledProc.type = OB_COMPILED;
    compiledProc.data = (unsigned long) Freep;
    *Freep++ = entry;
    *Freep++ = env;
    pop();
    return compiledProc;
error:
    return err;
}

Object compiled_procedure_entry(Object proc)
{
    return car(proc);
}

Object compiled_procedure_env(Object proc)
{
    return cdr(proc);
}

/* Garbagge collection subroutine */

Object *tospace = NULL;

Object *regs[] = {&expr, &val, &unev, &global_env, &env, &argl, &proc};

Object registerSymbolObject(Object symObj)
{
    char *str = (char *) &((Object *) symObj.data - heap + tospace)->data;
    /* debug("%s", str); */
    unsigned hashval = hash(str);
    Obnode *bucket = NULL;
    Obnode *prev = NULL;
    int cmpResult = 0;
    for (prev = &obarray[hashval], bucket = prev->next; bucket; prev = bucket, bucket = prev->next) {
        cmpResult = strcmp(str, getString(bucket->symbol));
        if (cmpResult == 0)
            return bucket->symbol;
        if (cmpResult < 0) {
            prev->next = malloc(sizeof (Obnode));
            prev->next->symbol = symObj;
            prev->next->next = bucket;
            return symObj;
        }
    }
    prev->next = malloc(sizeof (Obnode));
    prev->next->symbol = symObj;
    prev->next->next = bucket;
    return symObj;
}
/* relocate-old-result-in-new */
Object relocate_old_result_in_new(Object old)
{
    Object new;
    switch(old.type) {
        case OB_PAIR:
        case OB_STRING:
        case OB_SYMBOL:
        case OB_COMPILED:
            if (car(old).type == OB_BROKEN_HEART) {
                new = car(old);
                new.type = old.type;
                return new;
            }
            break;
        default:
            return old;
    }
    switch(old.type) {
        case OB_PAIR:
            new.type = old.type;
            new.data = (unsigned long) (Freep - tospace + heap);
            *Freep++ = car(old);
            *Freep++ = cdr(old);
            set_car(old, (Object) {.type = OB_BROKEN_HEART, .data = new.data});
            return new;
        case OB_SYMBOL:
        case OB_STRING: // fallthrough
            new.type = old.type;
            new.len = old.len;
            new.data = (unsigned long) (Freep - tospace + heap);
            Freep->type = OB_STRING_DATA;
            Freep->len = ((Object *)old.data)->len;
            memcpy((char *)&Freep->data, getString(old), new.len + 1);
            Freep += Freep->len;
            set_car(old,
                    (Object) {.type = OB_BROKEN_HEART,
                        .len = new.len,
                        .data = new.data});
            if (new.type == OB_SYMBOL)
                registerSymbolObject(new);
            return new;
        case OB_COMPILED:
            new.type = old.type;
            new.data = (unsigned long) (Freep - tospace + heap);
            *Freep++ = compiled_procedure_entry(old);
            *Freep++ = compiled_procedure_env(old);
            set_car(old, (Object) {.type = OB_BROKEN_HEART, .data = new.data});
            return new;
        default:
            sentinel("Cannot be called.");
    }
error:
    return err;
}

int gc(void)
{
    tospace = malloc(sizeof(Object) * word_size);
    check_mem(tospace);
    Object old;

    Object *scan = Freep = tospace;
    int i = 0;
    destroy_obarray();
    initialize_obarray();

    for (Object **opp = toUpdateStack; opp < _sp; opp++)
        **opp = relocate_old_result_in_new(**opp);

    for (i = 0; i < sizeof regs / sizeof regs[0]; i++)
        *regs[i] = relocate_old_result_in_new(*regs[i]);

    for (i = 0; i < current_depth; i++)
        stack[i] = relocate_old_result_in_new(stack[i]);

    /* gc-loop */
    while (scan < Freep) {
        old = *scan;
        if (old.type == OB_STRING_DATA) { // skip string datum
            scan += scan->len;
            continue;
        }
        *scan++ = relocate_old_result_in_new(old);
    }
    memcpy(heap, tospace, sizeof(Object) * (Freep - tospace));

    Freep = Freep - tospace + heap;

    free(tospace);
    tospace = heap; // for reuse registerObject

    set_default_symbols();

    return 1;
error:
    if (tospace)
        free(tospace);
    return 0;
}

void user_print(Object val)
{
    switch (val.type) {
        case OB_EXACT:
            fprintf(stdout, "%ld", (long) val.data);
            break;
        case OB_INEXACT:
            fprintf(stdout, "%g", *(double *) &val.data);
            break;
        case OB_SYMBOL:
            fprintf(stdout, "%s", getString(val));
            break;
        case OB_STRING:
            fprintf(stdout, "\"%s\"", getString(val));
            break;
        case OB_NIL:
            fprintf(stdout, "()");
            break;
        case OB_BOOLEAN:
            fprintf(stdout, (bool) val.data ? "#t" : "#f");
            break;
        case OB_PRIMITVE:
            fprintf(stdout, "<primitive procedure>");
            break;
        case OB_COMPILED:
            fprintf(stdout, "<compiled procedure>");
            break;
        case OB_PAIR:
            fprintf(stdout, "(");
            user_print(car(val));
            for (val = cdr(val); val.type == OB_PAIR; val = cdr(val)) {
                fprintf(stdout, " ");
                user_print(car(val));
            }
            if (isNull(val))
                fprintf(stdout, ")");
            else {
                fprintf(stdout, " . ");
                user_print(val);
                fprintf(stdout, ")");
            }
            break;
        default:
            sentinel("Unsupported expression, type num: %d", val.type);
    }
error: // fallthrough
    return;
}
