#include <Reader.h>
#include <Reader_internal.h>
#include <getChar.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <dbg_testable.h>

/* Memory allocation */

Object *heap = NULL;
unsigned long Free = 0;
unsigned long word_size = 0;

int heap_Create(unsigned long byte_size)
{
    Free = 0;
    word_size = (sizeof (Object) - 1 + byte_size) / sizeof (Object);
    heap = malloc(word_size * sizeof (Object));
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
    consCell.data = (double) Free;
    heap[Free++] = carCell;
    heap[Free++] = cdrCell;
    return consCell;
}

Object car(Object consCell)
{
    return heap[(unsigned long) consCell.data];
}

Object cdr(Object consCell)
{
    return heap[(unsigned long) consCell.data + 1];
}

Object set_car(Object consCell, Object newCar)
{
    heap[(unsigned long) consCell.data] = newCar;
    return isNull(ok) ? ok = intern(make_string_obj("ok")) : ok;
}

Object set_cdr(Object consCell, Object newCdr)
{
    heap[(unsigned long) consCell.data + 1] = newCdr;
    return isNull(ok) ? ok = intern(make_string_obj("ok")) : ok;
}

bool isString(Object cell)
{
    return cell.type == OB_STRING;
}

char *getString(Object cell)
{
    return (char *) &heap[(unsigned long) cell.data];
}

bool isSymbol(Object cell)
{
    return cell.type == OB_SYMBOL;
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

#define DEFAULT_OBARRY_SIZE 101
#define OBARRAY_SIZE (sizeof obarray / sizeof obarray[0])
static Object obarray[DEFAULT_OBARRY_SIZE];

void initialize_obarray(void)
{
    for (int i = 0; i < OBARRAY_SIZE; i++)
        obarray[i] = nil;
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

void add_binding_to_frame(Object var, Object val, Object frame)
{
    set_car(frame, cons(var, car(frame)));
    set_cdr(frame, cons(val, cdr(frame)));
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

Object make_string_obj(const char *cstring)
{
    unsigned long cstrlen = strlen(cstring);
    check(cstrlen <= LEN_MASK, "given string is too long.");

    Object str = {.type = OB_STRING, .len = cstrlen};
    size_t sizeInWords =
        (sizeof (Object) + str.len) / sizeof (Object);

    check(Free + sizeInWords < word_size,
          "Need garbage collect!");

    str.data = (double) Free;
    memcpy(&heap[Free], cstring, str.len + 1);
    Free += sizeInWords;
    return str;
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
            returnValue.data = (double) atol(token);
            rc = EOL;
            return returnValue;

        case INEXACT:
            returnValue.type = OB_INEXACT;
            returnValue.data =  atof(token);
            rc = EOL;
            return returnValue;

        case STRING:
            rc = EOL;
            return make_string_obj(token);

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

