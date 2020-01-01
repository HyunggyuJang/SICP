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
      CHECK(heap_Create(10000));
      CHECK(FormatOutSpy_Create(100));
      CHECK(stdoutBuf = FormatOutSpy_GetOutput(stdout));
      CHECK(stderrBuf = FormatOutSpy_GetOutput(stderr));
      initialize_obarray();
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
  GetCharSpy_Create("(1 . 2)");
  checkToken('(', "(");
  checkToken(EXACT, "1");
  checkToken('.', ".");
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
  DOUBLES_EQUAL(1234.578,  obRead.data, 0.0001);
}

TEST(Reader, ReadStringSmallSize)
{
  GetCharSpy_Create("\"This must fit\"");
  obRead = read();
  BYTES_EQUAL(OB_STRING, obRead.type);
  STRCMP_EQUAL("This must fit", (char *) &heap[(unsigned long) obRead.data]);
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
               (char *) &heap[(unsigned long) obRead.data]);
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
  DOUBLES_EQUAL(234.24, cdr(consCell).data, 0.0001);
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
  STRCMP_EQUAL("Hi, there", (char *) &heap[(unsigned long) car(obRead).data]);
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
  DOUBLES_EQUAL(.3, car(cdr(cdr(obRead))).data, 0.001);
}

// object test
TEST(Reader, ObjectSize)
{
  LONGS_EQUAL(sizeof(Object), 16);
}

TEST(Reader, MakeObjectFromCStringInternal)
{
  char testString[] = "TEST";
  Object str = {.type = OB_STRING, .len = strlen(testString)};
  str.data = (double) ~0U;
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
