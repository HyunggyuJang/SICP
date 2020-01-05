#include <Reader.h>
#include <Reader_internal.h>
#include <getChar.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <dbg_testable.h>
#include <stdarg.h>

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

void heap_Destory(void)
{
    if (heap)
        free(heap);
}

/* scheme types */
bool isNull(Object cell)
{
    return cell.type == OB_NIL;
}

bool isPair(Object cell)
{
    return cell.type == OB_PAIR || cell.type == OB_NIL;
}

Object cons(Object carCell, Object cdrCell)
{
    #ifdef GC_WORK
    if (!(Freep - heap + 2 < word_size)) {
        debug("Call garbage collector. -- CONS");
        check(gc(), "Failed to clean up garbage.");
    }
    check(Freep - heap + 2 < word_size, "Heap overflow. -- CONS");
    #endif
    Object consCell;
    consCell.type = OB_PAIR;
    consCell.data = (unsigned long) Freep;
    *Freep++ = carCell;
    *Freep++ = cdrCell;
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

bool isString(Object cell)
{
    return cell.type == OB_STRING;
}

char *getString(Object cell)
{
    return (char *) &((Object *)cell.data)->data;
}

bool isNumber(Object cell)
{
    return cell.type == OB_INEXACT || cell.type == OB_EXACT;
}

bool isSymbol(Object cell)
{
    return cell.type == OB_SYMBOL;
}

bool isSelfEvaluating(Object cell)
{
    return cell.type == OB_STRING || isNumber(cell) || isNull(cell);
}

bool eq(Object o1, Object o2)
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
            prev->next->symbol = make_string_obj(str);
            prev->next->symbol.type = OB_SYMBOL;
            prev->next->next = bucket;
            return prev->next->symbol;
        }
    }
    prev->next = malloc(sizeof (Obnode));
    prev->next->symbol = make_string_obj(str);
    prev->next->symbol.type = OB_SYMBOL;
    prev->next->next = bucket;
    return prev->next->symbol;
}
void registerSymbolObject(Object symObj) {
    char *str = getString(symObj);
    unsigned hashval = hash(str);
    Obnode *bucket = NULL;
    Obnode *prev = NULL;
    int cmpResult = 0;
    for (prev = &obarray[hashval], bucket = prev->next; bucket; prev = bucket, bucket = prev->next) {
        cmpResult = strcmp(str, getString(bucket->symbol));
        if (cmpResult == 0)
            return;
        if (cmpResult < 0) {
            prev->next = malloc(sizeof (Obnode));
            prev->next->symbol = symObj;
            prev->next->next = bucket;
            return;
        }
    }
    prev->next = malloc(sizeof (Obnode));
    prev->next->symbol = symObj;
    prev->next->next = bucket;
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
    for (len = 0; isPair(list) && !isNull(list);
         len++, list = cdr(list))
        ;
    return isNull(list) ? len : -(len + 1); // for non-list detection
}

Object add_binding_to_frame(Object var, Object val, Object frame)
{
    set_car(frame, cons(var, car(frame)));
    return set_cdr(frame, cons(val, cdr(frame)));
}

Object extend_frame(Object vars, Object vals, Object base_env)
{
    long var_len = length(vars);
    long val_len = length(vals);
    check(var_len == val_len || var_len < 0 && -var_len <= val_len,
          "number of variables and values should match; vars' %ld, vals' %ld",
          var_len, val_len);
    return cons(make_frame(vars, vals), base_env);

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
             isPair(vars) && !isNull(vars);
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
#ifdef GC_WORK
    if (!(Freep - heap + sizeInWords < word_size)) {
        debug("Call the garbage collector -- MAKE_STRING_OBJ.");
        check(gc(), "Failed to cleanup garbage.");
    }
    check(Freep - heap + sizeInWords < word_size,
          "Heap overflow -- MAKE_STRING_OBJ.");
#endif
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

Object read(void)
{
    static int rc = EOL;
    if (rc == EOL)
        rc = getToken();
    Object returnValue;
    check_debug(rc != EOL, "Nothing to read.");

    switch(rc) {
        case WORD:
            rc = EOL;
            return intern(token);

        case EXACT:
            returnValue.type = OB_EXACT;
            returnValue.data = atol(token);
            rc = EOL;
            return returnValue;

        case INEXACT:
            returnValue.type = OB_INEXACT;
            {
                // to work around
                double temp = atof(token);
                returnValue.data = *(long *) &temp;
            }
            rc = EOL;
            return returnValue;

        case STRING:
            rc = EOL;
            return make_string_obj(token);

        case '\'':
            rc = EOL;
            returnValue = read();
            check(!eq(returnValue, err),
                  "Expected expression but error.");
            return cons(quote, cons(returnValue, nil));

        case '(':
            rc = getToken();
            if (rc == '.') {
                rc = EOL;
                returnValue = read();
                rc = getToken();
                check(rc == ')', "Expected ) but %s", token);
                rc = EOL;
                return returnValue;
            }
            if (rc == ')') {
                rc = EOL;
                returnValue.type = OB_NIL;
                return returnValue;
            }
            returnValue = read();
            rc = '(';
            return cons(returnValue, read());

        case ')':
            sentinel("Expected expression but )");

        default:
            sentinel("Unsupported expression, token number: %d", rc);
    }

error: //fallthrough
    return err;
}

void user_print(Object val)
{
    switch (val.type) {
        case OB_EXACT:
            formatOut(stdout, "%ld", (long) val.data);
            break;
        case OB_INEXACT:
            formatOut(stdout, "%g", *(double *) &val.data);
            break;
        case OB_SYMBOL:
            formatOut(stdout, "%s", getString(val));
            break;
        case OB_STRING:
            formatOut(stdout, "\"%s\"", getString(val));
            break;
        case OB_NIL:
            formatOut(stdout, "()");
            break;
        case OB_BOOLEAN:
            formatOut(stdout, (bool) val.data ? "#t" : "#f");
            break;
        case OB_PRIMITVE:
            formatOut(stdout, "<primitive procedure>");
            break;
        case OB_COMPOUND:
            formatOut(stdout, "<compound procedure>");
        case OB_PAIR:
            formatOut(stdout, "(");
            user_print(car(val));
            for (val = cdr(val); val.type == OB_PAIR; val = cdr(val)) {
                formatOut(stdout, " ");
                user_print(car(val));
            }
            if (isNull(val))
                formatOut(stdout, ")");
            else {
                formatOut(stdout, " . ");
                user_print(val);
                formatOut(stdout, ")");
            }
            break;
        default:
            sentinel("Unsupported expression, type num: %d", val.type);
    }
error: // fallthrough
    return;
}

bool quoted_p(Object exp)
{
    return eq(quote, car(exp));
}

bool define_p(Object exp)
{
    return eq(define, car(exp));
}

Object text_of_quotation(Object exp)
{
    return car(cdr(exp));
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

void initialize_stack(void)
{
    current_depth = max_depth = 0;
}

Object primitive_procedure_names = {.type = OB_NIL};
Object primitive_procedure_objects = {.type = OB_NIL};

void addto_primitive_procedures(char *schemeName, primproc_t proc)
{
    primitive_procedure_names = cons(intern(schemeName),
                                     primitive_procedure_names);
    primitive_procedure_objects = cons(make_primitive_procedure(proc),
                                       primitive_procedure_objects);
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
        extend_frame(primitive_procedure_names,
                     primitive_procedure_objects,
                     the_empty_env);
    define_variable(true_s, sharp_t, global_env);
    define_variable(false_s, sharp_f, global_env);
}

Object expr, val, unev, env, argl, proc;
Object cont = {.type = OB_LABEL, .data = (unsigned long) NULL};

JUMP_LIST label(Object lab_obj){
    check(lab_obj.type == OB_LABEL, "label accept only label object.");
    return lab_obj.data;
error:
    return -1;
}

Object make_label(JUMP_LIST label) {
    return (Object) {.type = OB_LABEL, .data = (unsigned long)label};
}

void repl(void)
{
    while (true) {
        formatOut(stdout, "> ");
        expr = read();
        env = global_env;
        if (eq(expr, err))
            return;
        interpret();
        if (eq(val, err)) {
            formatOut(stderr, "REPL exited abnormally.");
            return;
        }
        formatOut(stdout, ";Value: ");
        user_print(val);
        formatOut(stdout, "\n");
    }
}
Object cadr(Object exp)
{
    return car(cdr(exp));
}
Object cddr(Object exp)
{
    return cdr(cdr(exp));
}
Object caddr(Object exp)
{
    return car(cddr(exp));
}
Object cdddr(Object exp)
{
    return cdr(cddr(exp));
}
Object cadddr(Object exp)
{
    return car(cdddr(exp));
}
Object caadr(Object exp)
{
    return car(cadr(exp));
}
Object cdadr(Object exp)
{
    return cdr(cadr(exp));
}
Object cadadr(Object exp)
{
    return cadr(cadr(exp));
}

Object make_lambda(Object params, Object body)
{
    return cons(lambda, cons(params, body));
}

Object definition_variable(Object exp)
{
    return isSymbol(cadr(exp)) ? cadr(exp) : caadr(exp);
}

Object definition_value(Object exp)
{
    return isSymbol(cadr(exp)) ? caddr(exp)
        : make_lambda(cdadr(exp), cddr(exp));
}

Object if_predicate(Object exp)
{
    return car(cdr(exp));
}

Object if_consequent(Object exp)
{
    return car(cdr(cdr(exp)));
}

Object if_alternative(Object exp)
{
    return car(cdr(cdr(cdr(exp))));
}

Object assignment_variable(Object exp)
{
    return cadr(exp);
}

Object assignment_value(Object exp)
{
    return caddr(exp);
}

bool true_p(Object exp)
{
    return !(exp.type == OB_BOOLEAN) || (bool) exp.data;
}

Object make_procedure(Object params, Object body, Object env)
{
    #ifdef GC_WORK
    if (!(Freep - heap + 3 < word_size)) {
        debug("Call garbage collector -- MAKE_PROCEDURE");
        check(gc(), "Failed to clean up garbage.");
    check(Freep - heap + 3 < word_size, "Heap overflow. -- MAKE_PROCEDURE");
    }
    #endif
    Object proc = {.type = OB_COMPOUND,
                   .data = (unsigned long) Freep};
    *Freep++ = params;
    *Freep++ = body;
    *Freep++ = env;
    return proc;
error:
    return err;
}

Object procedure_params(Object proc)
{
    return *(Object *)proc.data;
}

Object procedure_body(Object proc)
{
    return *((Object *)proc.data + 1);
}

Object procedure_env(Object proc)
{
    return *((Object *)proc.data + 2);
}

Object lambda_params(Object exp)
{
    return cadr(exp);
}

Object lambda_body(Object exp)
{
    return cddr(exp);
}

Object operator(Object exp)
{
    return car(exp);
}

Object operands(Object exp)
{
    return cdr(exp);
}

Object adjoin_arg(Object arg, Object list)
{
#if 1
    return cons(arg, list);
#else
    /* To expensive! */
    if (isNull(list))
        return cons(arg, list);
    return cons(car(list), adjoin_arg(arg, cdr(list)));
#endif
}

Object reverse(Object list)
{
    Object reversed = nil;
    for (;isPair(list) && !isNull(list); list = cdr(list))
        reversed = cons(car(list), reversed);
    return reversed;
}

Object begin_actions(Object exp)
{
    return cdr(exp);
}

/*
 * The labels needs dynamic jump
don
e
ev_sequence_continue
ev_appl_did_operator
ev_appl_accumulate_arg
ev_appl_accum_last_arg
ev_assignment_1
ev_if_decide
ev_definition_1
 * Which means, other than above, we can code the label with vanilla goto label in C;
 * without using GCC extension.
 */
void interpret(void)
{
    cont = make_label(done);
eval:
    #ifdef GC_ALTERNATIVE
    if (word_size - (Freep - heap) < word_size / 10)
        gc();
    check(!(word_size - (Freep - heap) < word_size / 10), "Heap got full.");
    #endif
    switch(expr.type) {
        case OB_EXACT: // self evaluation
        case OB_INEXACT:
        case OB_NIL:
        case OB_STRING:
            val = expr;
            goto jump;
        case OB_SYMBOL:
            val = lookup_variable_value(expr, env);
            goto jump;
        case OB_PAIR:
        {
            Object type = car(expr);
            if (eq(type, quote)) {
                val = text_of_quotation(expr);
                goto jump;
            }
            if (eq(type, define))
                goto ev_definition;

            if (eq(type, if_s))
                goto ev_if;

            if (eq(type, set_bang))
                goto ev_assignment;

            if (eq(type, lambda))
                goto ev_lambda;

            if (eq(type, begin))
                goto ev_begin;

            goto ev_application;
        }
        default:
            sentinel("Unknown expression, type num %d", expr.type);
    }
ev_begin:
    unev = begin_actions(expr);
    save(cont);
    goto ev_sequence;
ev_sequence:
    expr = car(unev);
    if (isNull(cdr(unev)))
        goto ev_sequence_last_exp;
    save(unev);
    save(env);
    cont = make_label(ev_sequence_continue);
    goto eval;
ev_sequence_last_exp:
    cont = restore();
    goto eval;
ev_application:
    save(cont);
    save(env);
    save(operands(expr));
    expr = operator(expr);
    cont = make_label(ev_appl_did_operator);
    goto eval;
ev_appl_operand_loop:
    save(argl);
    expr = car(unev);
    if (isNull(cdr(unev)))
        goto ev_appl_last_arg;
    save(env);
    save(unev);
    cont = make_label(ev_appl_accumulate_arg);
    goto eval;
ev_appl_last_arg:
    cont = make_label(ev_appl_accum_last_arg);
    goto eval;
ev_lambda:
    val = make_procedure(lambda_params(expr),
                         lambda_body(expr), env);
    check(!eq(val, err), "Error occured in processing ev_lambda.");
    goto jump;
ev_assignment:
    save(assignment_variable(expr));
    expr = assignment_value(expr);
    save(env);
    save(cont);
    cont = make_label(ev_assignment_1);
    goto eval;
ev_if:
    save(expr);
    save(env);
    save(cont);
    cont = make_label(ev_if_decide);
    expr = if_predicate(expr);
    goto eval;
ev_definition:
    save(definition_variable(expr));
    expr = definition_value(expr);
    save(env);
    save(cont);
    cont = make_label(ev_definition_1);
    goto eval;
apply_dispatch:
    switch(proc.type) {
        case OB_PRIMITVE:
            val = apply_primitive_procedure(proc, argl);
            check(!eq(val, err), "Error occured during ev_appl_primitive.");
            cont = restore();
            goto jump;
        case OB_COMPOUND:
            unev = procedure_params(proc);
            env = procedure_env(proc);
            env = extend_frame(unev, argl, env);
            check(!eq(env, err), "Failed to extend frame in compound application.");
            unev = procedure_body(proc);
            goto ev_sequence;
        default:
            formatOut(stderr, ";The object ");
            user_print(proc);
            formatOut(stderr, " is not applicable.");
            goto error;
    }
jump:
    switch(label(cont)) {
        case done:
            return;
        case ev_sequence_continue:
            env = restore();
            unev = restore();
            unev = cdr(unev);
            goto ev_sequence;
        case ev_appl_did_operator:
            unev = restore();
            env = restore();
            argl = nil;
            proc = val;
            check(!eq(val, err), "Error in ev_appl_did_operator.");
            if (isNull(unev))
                goto apply_dispatch;
            save(proc);
            goto ev_appl_operand_loop;
        case ev_appl_accumulate_arg:
            unev = restore();
            env = restore();
            argl = restore();
            argl = adjoin_arg(val, argl);
            check(!eq(argl, err), "Error in ev_appl_accumulate_arg.");
            unev = cdr(unev);
            goto ev_appl_operand_loop;
        case ev_appl_accum_last_arg:
            argl = restore();
            argl = adjoin_arg(val, argl);
            check(!eq(argl, err), "Error in ev_appl_last_arg.");
            argl = reverse(argl);
            proc = restore();
            goto apply_dispatch;
        case ev_assignment_1:
            cont = restore();
            env = restore();
            unev = restore();
            val = set_variable_value(unev, val, env);
            check(!eq(val, err), "Error in ev_assignment_1.");
            goto jump;
        case ev_if_decide:
            cont = restore();
            env = restore();
            expr = restore();
            if (true_p(val))
                goto ev_if_consequent;
        ev_if_alternative:
            expr = if_alternative(expr);
            goto eval;
        ev_if_consequent:
            expr = if_consequent(expr);
            goto eval;
        case ev_definition_1:
            cont = restore();
            env = restore();
            unev = restore();
            val = define_variable(unev, val, env);
            goto jump;
    }
error:
    val = err;
    return;
}

#if defined(GC_ALTERNATIVE) || defined(GC_WORK)
/* Garbagge collection subroutine */
Object *regs[] = {&expr, &val, &unev, &global_env, &env, &argl, &proc};

/* relocate-old-result-in-new */
Object relocate_old_result_in_new(Object old)
{
    Object new;
    switch(old.type) {
        case OB_PAIR:
        case OB_STRING:
        case OB_SYMBOL:
        case OB_COMPOUND:
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
            new.data = (unsigned long) Freep;
            *Freep++ = car(old);
            *Freep++ = cdr(old);
            set_car(old, (Object) {.type = OB_BROKEN_HEART, .data = new.data});
            return new;
        case OB_SYMBOL:
        case OB_STRING: // fallthrough
            new.type = old.type;
            new.len = old.len;
            new.data = (unsigned long) Freep;
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
        case OB_COMPOUND:
            new.type = old.type;
            new.data = (unsigned long) Freep;
            *Freep++ = procedure_params(old);
            *Freep++ = procedure_body(old);
            *Freep++ = procedure_env(old);
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
    Object *tospace = malloc(sizeof (Object) * word_size);
    check_mem(tospace);

    Object new, old;
    Object *scan = Freep = tospace;
    int i = 0;
    destroy_obarray();
    initialize_obarray();

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

    free(heap);
    heap = tospace;

    set_default_symbols();

    return 1;
error:
    if (tospace)
        free(tospace);
    return 0;
}
#endif


/* token manager */
char token[100];

static bool isDelimiter(int c)
{
    return c == EOF || isspace(c) || c == '(' || c == ')' || c == '\'';
}

int getToken()
{
    int i = 0;
    int c = 0;

    while (isspace(c = getChar()))
        ;

    if (c == EOF) {
        token[i] = '\0';
        return EOL;
    }

    if (c == '(' || c == ')') {
        token[i++] = (char) c;
        token[i] = '\0';
        return c;
    }

    if (c == '\'') {
        token[i++] = (char) c;
        token[i] = '\0';
        return c;
    }

    if (c == '.') {
        if (isDelimiter(c = getChar())) {
            token[i++] = '.';
            token[i] = '\0';
            unGetc(c);
            return '.';
        }
        unGetc(c);
        c = '.';
        goto accumulateInexact;
    }

    if (c == '"') {
        for (i = 0, c = getChar(); c !=EOF && c != '"'; i++, c = getChar())
            token[i] = (char) c;
        check(c != EOF, "Unmatched quotation.")
        token[i] = '\0';
        return STRING;
    }

    if (isdigit(c)) {
        i = 0;
        do {
            token[i++] = (char) c;
        } while (isdigit(c = getChar()));

        token[i] = '\0';

        if (isDelimiter(c)) {
            unGetc(c);
            return EXACT;
        }

        if (c == '.') {
        accumulateInexact:
            do {
                token[i++] = (char) c;
            } while (isdigit(c = getChar()));

            token[i] = '\0';

            if (isDelimiter(c)) {
                unGetc(c);
                return INEXACT;
            }
        }
    }

    for (; !isDelimiter(c); c = getChar(), i++)
        token[i] = (char) c;
    unGetc(c);
    token[i] = '\0';
    return WORD;

error:
    token[i] = '\0';
    return ERR;
}

