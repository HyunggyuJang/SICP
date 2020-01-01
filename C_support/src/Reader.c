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
    heap = malloc(word_size);
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

bool isString(Object cell)
{
    return cell.type == OB_STRING;
}

char *getString(Object cell)
{
    return (char *) &heap[(unsigned long) cell.data];
}

// end of memory allocation

Object read(void)
{
    static int rc = EOL;
    if (rc == EOL)
        rc = getToken();
    Object returnValue;
    check_debug(rc != EOL, "Nothing to read.");

    switch(rc) {
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
            returnValue.type =  OB_STRING;
            returnValue.data = (double) Free;
            memcpy(&heap[Free], token, strlen(token) + 1);
            Free += ((sizeof (Object) + strlen(token)) / sizeof (Object));
            rc = EOL;
            return returnValue;

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
    returnValue.type = OB_ERR;
    return returnValue;
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

    for (; c != EOF && !isspace(c); c = getChar(), i++)
        token[i] = (char) c;
    token[i] = '\0';
    return WORD;

error:
    token[i] = '\0';
    return ERR;
}

