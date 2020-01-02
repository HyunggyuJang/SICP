#include <Reader.h>
#include <Reader_internal.h>
#include <getChar.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <dbg_testable.h>
#include <setjmp.h>

/* Memory allocation */
Object *heap = NULL;
#ifdef TOLONG
Object *Freep = NULL;
#else
unsigned long Free = 0;
#endif
unsigned long word_size = 0;

int heap_Create(unsigned long byte_size)
{
    word_size = (sizeof (Object) - 1 + byte_size) / sizeof (Object);
    heap = malloc(word_size * sizeof (Object));
#ifdef TOLONG
    Freep = heap;
#else
    Free = 0;
#endif
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
    Object consCell;
    consCell.type = OB_PAIR;
    #ifdef TOLONG
    consCell.data = Freep;
    *Freep++ = carCell;
    *Freep++ = cdrCell;
    #else
    consCell.data = (double) Free;
    heap[Free++] = carCell;
    heap[Free++] = cdrCell;
    #endif
    return consCell;
}

Object car(Object consCell)
{
#ifdef TOLONG
    return *((Object *) consCell.data);
#else
    return heap[(unsigned long) consCell.data];
#endif
}

Object cdr(Object consCell)
{
#ifdef TOLONG
    return *((Object *) consCell.data + 1);
#else
    return heap[(unsigned long) consCell.data + 1];
#endif
}

Object set_car(Object consCell, Object newCar)
{
#ifdef TOLONG
    *((Object *) consCell.data) = newCar;
#else
    heap[(unsigned long) consCell.data] = newCar;
#endif
    return ok;
}

Object set_cdr(Object consCell, Object newCdr)
{
#ifdef TOLONG
    *((Object *) consCell.data + 1) = newCdr;
#else
    heap[(unsigned long) consCell.data + 1] = newCdr;
#endif
    return ok;
}

bool isString(Object cell)
{
    return cell.type == OB_STRING;
}

char *getString(Object cell)
{
#ifdef TOLONG
    return (char *) cell.data;
#else
    return (char *) &heap[(unsigned long) cell.data];
#endif
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

// obarray
Object nil = {.type = OB_NIL};
Object err = {.type = OB_ERR};
Object ok = {.type = OB_NIL};
Object quote = {.type = OB_NIL};

#define DEFAULT_OBARRY_SIZE 101
#define OBARRAY_SIZE (sizeof obarray / sizeof obarray[0])
static Object obarray[DEFAULT_OBARRY_SIZE];

void initialize_obarray(void)
{
    for (int i = 0; i < OBARRAY_SIZE; i++)
        obarray[i] = nil;
    ok = intern(make_string_obj("ok"));
    quote = intern(make_string_obj("quote"));
}

static unsigned hash(char *s)
{
    unsigned hashval;

    for (hashval = 0; *s != '\0'; s++)
        hashval = *s + 31 * hashval;
    return hashval % OBARRAY_SIZE;
}

Object intern(Object str)
{
    char *retrievedStr = getString(str);
    unsigned hashval = hash(retrievedStr);
    Object bucket = { 0 };
    for (bucket = obarray[hashval]; !isNull(bucket); bucket = cdr(bucket)) {
        if (strcmp(getString(car(bucket)), retrievedStr) == 0)
            return car(bucket);
    }

    str.type = OB_SYMBOL;
    obarray[hashval] = cons(str, obarray[hashval]);
    return str;
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

static unsigned int length(Object list)
{
    int len = 0;
    for (len = 0; isPair(list) && !isNull(list);
         len++, list = cdr(list))
        ;
    return len;
}

Object add_binding_to_frame(Object var, Object val, Object frame)
{
    set_car(frame, cons(var, car(frame)));
    return set_cdr(frame, cons(val, cdr(frame)));
}

Object extend_frame(Object vars, Object vals, Object base_env)
{
    check(length(vars) == length(vals),
          "number of variables and values should match.");
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
             !isNull(vars);
             vars = cdr(vars), vals = cdr(vals))
            if (eq(var, car(vars)))
                return car(vals);
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
    size_t sizeInWords =
        (sizeof (Object) + str.len) / sizeof (Object);

#ifdef TOLONG
    check(Freep - heap + sizeInWords < word_size,
          "Need garbage collect!");
    str.data = (unsigned long)Freep;
    memcpy(Freep, cstring, str.len + 1);
    Freep += sizeInWords;
#else
    check(Free + sizeInWords < word_size,
          "Need garbage collect!");
    str.data = (double) Free;
    memcpy(&heap[Free], cstring, str.len + 1);
    Free += sizeInWords;
#endif
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
#ifdef TOLONG
            args.data = 0L;
#else
            args.data = (double) 0L;
#endif
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
#ifdef TOLONG
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
#else
            args.data =
                (augend.type == OB_EXACT ?
                 (long) augend.data :
                 augend.data)
                + (addend.type == OB_EXACT ?
                 (long) addend.data :
                   addend.data);
#endif
            return args;
        }
        default:
            sentinel("+ need number arguments.");
    }

error:
    return err;
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
            return intern(make_string_obj(token));

        case EXACT:
            returnValue.type = OB_EXACT;
#ifdef TOLONG
            returnValue.data = atol(token);
#else
            returnValue.data = (double) atol(token);
#endif
            rc = EOL;
            return returnValue;

        case INEXACT:
            returnValue.type = OB_INEXACT;
#ifdef TOLONG
            {
                // to work around
                double temp = atof(token);
                returnValue.data = *(long *) &temp;
            }
#else
            returnValue.data =  atof(token);
#endif
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
#ifdef TOLONG
            formatOut(stdout, "%ld", (long) val.data);
#else
            formatOut(stdout, "%ld", (long) val.data);
#endif
            break;
        case OB_INEXACT:
#ifdef TOLONG
            formatOut(stdout, "%g", *(double *) &val.data);
#else
            formatOut(stdout, "%g", val.data);
#endif
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
        case OB_PAIR:
            formatOut(stdout, "(");
            user_print(car(val));
            for (val = cdr(val); isPair(val); val = cdr(val)) {
                formatOut(stdout, " ");
                user_print(car(val));
            }
            if (isNull(val))
                formatOut(stdout, ")");
            else {
                formatOut(stdout, " . ");
                user_print(val);
            }
            break;
        default:
            sentinel("Unsupported expression, type num: %d", val.type);
    }
error: // fallthrough
    return;
}

void interpret(void)
{
    Object exp, val;
    jmp_buf cont_buf;
    int cont = 0;
    switch(setjmp(cont_buf)) {
        case PRINT_RESULT:
            formatOut(stdout, ";Value: ");
            user_print(val);
            formatOut(stdout, "\n");
        case READ_EVAL_PRINT_LOOP:
            formatOut(stdout, "> ");
            exp = read();
            if (eq(exp, err))
                return;
            cont = PRINT_RESULT;
            goto eval;
    }
eval:
    switch(exp.type) {
        case OB_EXACT:
        case OB_INEXACT:
        case OB_NIL:
        case OB_STRING:
            val = exp;
            longjmp(cont_buf, cont);
        default:
            sentinel("Unknown expression, type num %d", exp.type);
    }
error: //fallthrough
    return;
}

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
            do {
            accumulateInexact:
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

