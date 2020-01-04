extern "C" {
#include <stdio.h>
#include "../include/Reader.h"
#include "../include/Reader_internal.h"
#include "../mocks/GetCharSpy.h"
#include "../mocks/FormatOutSpy.h"
#include <string.h>
}

#include <CppUTest/TestHarness.h>
TEST_GROUP(Reader)
{
  char *stdoutBuf = NULL;
  char *stderrBuf = NULL;
  Object obRead;
    void setup()
    {
      CHECK(heap_Create(30000));
      CHECK(FormatOutSpy_Create(100));
      CHECK(stdoutBuf = FormatOutSpy_GetOutput(stdout));
      CHECK(stderrBuf = FormatOutSpy_GetOutput(stderr));
      initialize_obarray();
      setup_environment();
      initialize_stack();
    }

    void teardown()
    {
      FormatOutSpy_Destroy();
      heap_Destory();
    }

    void checkToken(int expectedReturn, const char *expectedToken)
    {
      int rc = getToken();
      LONGS_EQUAL(expectedReturn, rc);
      STRCMP_EQUAL(expectedToken, token);
    }
};

// Token test

TEST(Reader, GetTokenNothing)
{
  GetCharSpy_Create("");
  checkToken(EOL, "");
}

TEST(Reader, GetTokenExact)
{
  GetCharSpy_Create("1234");
  checkToken(EXACT, "1234");
}

TEST(Reader, GetTokenInexact)
{
  GetCharSpy_Create("123.45");
  checkToken(INEXACT, "123.45");
}

TEST(Reader, GetTokenWord)
{
  GetCharSpy_Create("12+34IsWord");
  checkToken(WORD, "12+34IsWord");
}

TEST(Reader, GetTokenParen)
{
  GetCharSpy_Create("(1 a)");
  checkToken('(', "(");
  checkToken(EXACT, "1");
  checkToken(WORD, "a");
  checkToken(')', ")");
}

TEST(Reader, GetTokenPair)
{
  GetCharSpy_Create("(1 . 2)(1 2)");
  checkToken('(', "(");
  checkToken(EXACT, "1");
  checkToken('.', ".");
  checkToken(EXACT, "2");
  checkToken(')', ")");
  checkToken('(', "(");
  checkToken(EXACT, "1");
  checkToken(EXACT, "2");
  checkToken(')', ")");
}

TEST(Reader, GetTokenSkipWhiteSpaces)
{
  GetCharSpy_Create("    \n(\n  )");
  checkToken('(', "(");
  checkToken(')', ")");
  checkToken(EOL, "");
}

TEST(Reader, GetTokenString)
{
  GetCharSpy_Create("\"This is \nstring!\" "
                    "\"(This is also)\"");
  checkToken(STRING, "This is \nstring!");
  checkToken(STRING, "(This is also)");
}

TEST(Reader, GetTokenStringUnmatched)
{
  GetCharSpy_Create("\"This is unmatched string.");
  checkToken(ERR, "This is unmatched string.");
  STRCMP_CONTAINS("Unmatched quotation.", stderrBuf);
}

TEST(Reader, GetTokenSymbol)
{
  GetCharSpy_Create("   '  HI?THISisSymbol!    ");
  checkToken('\'', "'");
  checkToken(WORD, "HI?THISisSymbol!");
}
// end token test

TEST(Reader, ReadExact)
{
  GetCharSpy_Create("1234");
  obRead = read();
  BYTES_EQUAL(OB_EXACT, obRead.type);
  LONGS_EQUAL(1234L, (long) obRead.data);
}

TEST(Reader, ReadInexact)
{
  GetCharSpy_Create("1234.578");
  obRead = read();
  BYTES_EQUAL(OB_INEXACT, obRead.type);
  DOUBLES_EQUAL(1234.578, *(double *)&obRead.data, 0.0001);
}

TEST(Reader, ReadStringSmallSize)
{
  GetCharSpy_Create("\"This must fit\"");
  obRead = read();
  BYTES_EQUAL(OB_STRING, obRead.type);
  STRCMP_EQUAL("This must fit", getString(obRead));
}

TEST(Reader, ReadStringArbitrarySize)
{
  GetCharSpy_Create("\"This relatively loooooooooooonnnng"
                    " line also should fit in heap. \"");
  obRead = read();
  CHECK(obRead.type != OB_ERR);
  STRCMP_EQUAL("", stderrBuf);
  BYTES_EQUAL(OB_STRING, obRead.type);
  STRCMP_EQUAL("This relatively loooooooooooonnnng"
               " line also should fit in heap. ",
               getString(obRead));
}

TEST(Reader, ReadSymbol)
{
  GetCharSpy_Create("test test");

  obRead = read();
  CHECK(isSymbol(obRead));

  Object obRead2 = read();
  CHECK(isSymbol(obRead2));

  CHECK(eq(obRead, obRead2));
}

// Memory allocation test

TEST(Reader, ConsCarCdrContraction)
{
  GetCharSpy_Create("1234 234.24");
  Object consCell = cons(read(), read());
  CHECK(isPair(consCell));
  LONGS_EQUAL(1234, (long) car(consCell).data);
  Object cdrCell = cdr(consCell);
  DOUBLES_EQUAL(234.24, *(double *)&cdrCell.data, 0.0001);
}

TEST(Reader, ReadListNil)
{
  GetCharSpy_Create("()");
  obRead = read();
  CHECK(isPair(obRead));
}

TEST(Reader, ReadListWithArbitrary)
{
  GetCharSpy_Create("(1 2 3 4 \"Hi, there\")");
  obRead = read();
  CHECK(isPair(obRead));
  for (int i = 1; i < 5; i++, obRead = cdr(obRead))
    LONGS_EQUAL(i, (long) car(obRead).data);
  STRCMP_EQUAL("Hi, there", getString(car(obRead)));
}

TEST(Reader, ReadTree)
{
  GetCharSpy_Create("((1 2) 3 (4))");
  obRead = read();
  CHECK(isPair(obRead));
  int i = 0;
  Object current = { 0 };
  CHECK(isPair(car(obRead)));
  for (i = 1, current = car(obRead); i < 3; i++, current = cdr(current))
    LONGS_EQUAL(i, (long) car(current).data);
  LONGS_EQUAL(3, (long) car(cdr(obRead)).data);
  LONGS_EQUAL(4, (long) car(car(cdr(cdr(obRead)))).data);
}

TEST(Reader, ReadPair)
{
  GetCharSpy_Create("(1 . (2 . 3))");
  obRead = read();
  CHECK(isPair(obRead));
  LONGS_EQUAL(1, (long) car(obRead).data);
  LONGS_EQUAL(2, (long) car(cdr(obRead)).data);
  LONGS_EQUAL(3, (long) cdr(cdr(obRead)).data);
}

TEST(Reader, ReadPairLikeDouble)
{
  GetCharSpy_Create("(1 . (2 .3))");
  obRead = read();
  CHECK(isPair(obRead));
  LONGS_EQUAL(1, (long) car(obRead).data);
  LONGS_EQUAL(2, (long) car(cdr(obRead)).data);
  Object caddrCell = car(cdr(cdr(obRead)));
  DOUBLES_EQUAL(.3, *(double*)&caddrCell.data, 0.001);
}

TEST(Reader, ReadQuoteExpansion)
{
  GetCharSpy_Create("'a");
  obRead = read();
  CHECK(isPair(obRead));
  STRCMP_EQUAL("quote", getString(car(obRead)));
  STRCMP_EQUAL("a", getString(car(cdr(obRead))));
  CHECK(isNull(cdr(cdr(obRead))));

  GetCharSpy_Create("'(1 2)");
  obRead = read();
  CHECK(isPair(obRead));
  STRCMP_EQUAL("quote", getString(car(obRead)));
  LONGS_EQUAL(1, (long) car(car(cdr(obRead))).data);
  LONGS_EQUAL(2, (long) car(cdr(car(cdr(obRead)))).data);
}

// object test
TEST(Reader, ObjectSize)
{
  LONGS_EQUAL(sizeof(Object), 16);
}

TEST(Reader, DoubleToLong)
{
  double from = 5.231;
  long to = *(long *) &from;
  from = 2.341;
  DOUBLES_EQUAL(5.231, *(double*) &to, 0.0001);
}

TEST(Reader, SignedLongToUnsignedLong)
{
  long from = -1L;
  unsigned long to = from;
  LONGS_EQUAL(from, (long) to);
}

TEST(Reader, MakeObjectFromCStringInternal)
{
  char testString[] = "TEST";
  Object str = {.type = OB_STRING, .len = strlen(testString)};
  str.data =  ~0U;
  LONGS_EQUAL(4, str.len);
  LONGS_EQUAL(~0U, (unsigned long) str.data);
}

TEST(Reader, SetCarAndSetCdr)
{
  GetCharSpy_Create("(one two)");
  obRead = read();
  STRCMP_EQUAL("ok",
               getString(set_cdr(cdr(cdr(obRead)),
                       make_string_obj("three"))));
  STRCMP_EQUAL("three",
               getString(cdr(cdr(cdr(obRead)))));
}

TEST(Reader, EqTest)
{
  GetCharSpy_Create("(1 test? test? 1)");
  obRead = read();
  CHECK(eq(car(obRead), car(cdr(cdr(cdr(obRead))))));
  CHECK(eq(car(cdr(obRead)), (car(cdr(cdr(obRead))))));
}

TEST(Reader, PrimitiveProcedures)
{
  GetCharSpy_Create("(1 .3 5)");
  obRead = plus(read());
  DOUBLES_EQUAL(6.3, *(double *)&obRead.data, 0.0001);
}

// environment test
TEST(Reader, EnvironmentRetrieve)
{
  GetCharSpy_Create("(a b c) (1 2 3)");
  Object vars = read();
  Object vals = read();
  Object env = extend_frame(vars, vals, the_empty_env);
  LONGS_EQUAL(1,
              (long) lookup_variable_value(car(vars), env).data);
  LONGS_EQUAL(2,
              (long) lookup_variable_value(car(cdr(vars)), env).data);
  LONGS_EQUAL(3,
              (long) lookup_variable_value(car(cdr(cdr(vars))), env).data);
}

TEST(Reader, SetupGlobal)
{
  GetCharSpy_Create("+ car cdr set-car! set-cdr! cons true false");
  Object var = read();
  BYTES_EQUAL(OB_PRIMITVE, lookup_variable_value(var, global_env).type);
  var = read();
  BYTES_EQUAL(OB_PRIMITVE, lookup_variable_value(var, global_env).type);
  var = read();
  BYTES_EQUAL(OB_PRIMITVE, lookup_variable_value(var, global_env).type);
  var = read();
  BYTES_EQUAL(OB_PRIMITVE, lookup_variable_value(var, global_env).type);
  var = read();
  BYTES_EQUAL(OB_PRIMITVE, lookup_variable_value(var, global_env).type);
  var = read();
  BYTES_EQUAL(OB_PRIMITVE, lookup_variable_value(var, global_env).type);
  var = read();
  BYTES_EQUAL(OB_BOOLEAN, lookup_variable_value(var, global_env).type);
  CHECK(lookup_variable_value(var, global_env).data);
  var = read();
  BYTES_EQUAL(OB_BOOLEAN, lookup_variable_value(var, global_env).type);
  CHECK(!lookup_variable_value(var, global_env).data);
}

// stack test
TEST(Reader, StackSaveAndRestore)
{
  GetCharSpy_Create("test (1 2)");
  BYTES_EQUAL(OB_ERR, restore().type);
  STRCMP_CONTAINS("No element in stack.", stderrBuf);
  save(read());
  save(read());
  LONGS_EQUAL(1, (long) car(restore()).data);
  STRCMP_EQUAL("test", getString(restore()));
  BYTES_EQUAL(OB_ERR, restore().type);
  STRCMP_CONTAINS("No element in stack.", stderrBuf);
}

//  interpreter test
TEST(Reader, InterpretSelfEval)
{
  GetCharSpy_Create("5.34 5 () \"String Test\"");
  repl();
  STRCMP_CONTAINS("5.34", stdoutBuf);
  STRCMP_CONTAINS("5", stdoutBuf);
  STRCMP_CONTAINS("()", stdoutBuf);
  STRCMP_CONTAINS("\"String Test\"", stdoutBuf);
}

TEST(Reader, InterpretQuoted)
{
  GetCharSpy_Create("'test '(1 2 test) '(1 . 2)"
                    "'((1 2 . 3) (a . b))");
  repl();
  STRCMP_CONTAINS("test", stdoutBuf);
  STRCMP_CONTAINS("(1 2 test)", stdoutBuf);
  STRCMP_CONTAINS("(1 . 2)", stdoutBuf);
  STRCMP_CONTAINS("((1 2 . 3) (a . b))", stdoutBuf);
}

TEST(Reader, InterpretDefineAndRetrieve)
{
  GetCharSpy_Create("(define x 5) x");
  repl();
  STRCMP_CONTAINS(";Value: ok", stdoutBuf);
  STRCMP_CONTAINS(";Value: 5", stdoutBuf);
}

TEST(Reader, InterpretIf)
{
  GetCharSpy_Create("(define x false)"
                    "(if x \"Hi\" \"Hello\")");
  repl();
  STRCMP_CONTAINS(";Value: \"Hello\"", stdoutBuf);
}

TEST(Reader, InterpretSet)
{
  GetCharSpy_Create("(define x 5) x (set! x 7) x");
  repl();
  STRCMP_CONTAINS(";Value: 5", stdoutBuf);
  STRCMP_CONTAINS(";Value: 7", stdoutBuf);
}

TEST(Reader, ApplyPrimitive)
{
  GetCharSpy_Create("+ (5 7 5.3)");
  obRead = lookup_variable_value(read(), global_env);
  BYTES_EQUAL(OB_PRIMITVE, obRead.type);
  Object calculated = apply_primitive_procedure(obRead, read());
  DOUBLES_EQUAL(17.3, *(double *)&calculated.data, 0.00001);
}

TEST(Reader, InterpretPrimitiveApplication)
{
  GetCharSpy_Create("(+ 1 2) (cons 1 2)");
  repl();
  STRCMP_CONTAINS(";Value: 3", stdoutBuf);
  STRCMP_CONTAINS(";Value: (1 . 2)", stdoutBuf);
}

TEST(Reader, InterpretCompoundApplication)
{
  GetCharSpy_Create("((lambda (y) (+ y y y)) 7)");
  repl();
  STRCMP_CONTAINS(";Value: 21", stdoutBuf);
}

TEST(Reader, InterpretProcedureDefinition)
{
  GetCharSpy_Create("(define (x y) (+ y y y)) (x 7)");
  repl();
  STRCMP_CONTAINS(";Value: 21", stdoutBuf);
}

TEST(Reader, InterpretBegin)
{
  GetCharSpy_Create("(begin (define x 6) (set! x 8) x)");
  repl();
  STRCMP_CONTAINS(";Value: 8", stdoutBuf);
}

TEST(Reader, SupportDotNotationInProcedure)
{
  GetCharSpy_Create("(define (list . x) x)"
                    "(list 1 2 3 4 5)"
                    "(list (+ 2 3 4))"
  );
  repl();
  STRCMP_CONTAINS(";Value: ok", stdoutBuf);
  STRCMP_CONTAINS(";Value: (1 2 3 4 5)", stdoutBuf);
  STRCMP_CONTAINS(";Value: (9)", stdoutBuf);
}

TEST(Reader, SupportMorePrimitives)
{
  GetCharSpy_Create("(if (< 5 1.28) true false) (* 5 .28 2)");
  repl();
  STRCMP_CONTAINS(";Value: #f", stdoutBuf);
  STRCMP_CONTAINS(";Value: 2.8", stdoutBuf);
}

TEST(Reader, TestIterativeProcedure)
{
  GetCharSpy_Create("(define (factorial n)"
                    "(define (iter count result)"
                    "(if (= count 1)"
                    "result"
                    "(iter (- count 1) (* result count))))"
                    "(iter n 1))");
  repl();
  GetCharSpy_Create("(factorial 5)");
  repl();
  STRCMP_CONTAINS(";Value: 120", stdoutBuf);
  int current_max_depth = max_depth;

  GetCharSpy_Create("(factorial 10)");
  repl();
  STRCMP_CONTAINS(";Value: 3628800", stdoutBuf);
  LONGS_EQUAL(current_max_depth, max_depth);

}

TEST(Reader, TestFibonacci_NeedGarbageCollection)
{
  GetCharSpy_Create("(define (fib n)"
                    "(if (< n 2) n"
                    "(+ (fib (- n 1)) (fib (- n 2)))))"
                    "(fib 11)");
  repl();
  STRCMP_CONTAINS("Need to collect garbage", stderrBuf);
  // STRCMP_CONTAINS(";Value: 89", stdoutBuf);
}
