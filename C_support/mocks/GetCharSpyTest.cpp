extern "C"
{
#include <stdio.h>
#include "GetCharSpy.h"
#include "../include/getChar.h"
}

#include "CppUTest/TestHarness.h"

TEST_GROUP(GetCharSpy)
{
    void setup()
    {
    }

    void teardown()
    {
       GetCharSpy_Destroy();
    }
};

TEST(GetCharSpy, Get)
{
  GetCharSpy_Create("Test 1234");
  BYTES_EQUAL('T', getChar());
  BYTES_EQUAL('e', getChar());
  BYTES_EQUAL('s', getChar());
  BYTES_EQUAL('t', getChar());
  BYTES_EQUAL(' ', getChar());
  BYTES_EQUAL('1', getChar());
  BYTES_EQUAL('2', getChar());
  BYTES_EQUAL('3', getChar());
  BYTES_EQUAL('4', getChar());
  LONGS_EQUAL(EOF, getChar());
  LONGS_EQUAL(EOF, getChar());
  LONGS_EQUAL(EOF, getChar());
}

TEST(GetCharSpy, CanUnGetc)
{
  GetCharSpy_Create("Hi, ther");
  BYTES_EQUAL('H', getChar());
  BYTES_EQUAL('i', getChar());
  BYTES_EQUAL(',', getChar());
  BYTES_EQUAL(' ', getChar());
  BYTES_EQUAL('t', getChar());
  BYTES_EQUAL('h', getChar());

  int readChar = getChar();

  BYTES_EQUAL('e', readChar);
  BYTES_EQUAL('r', getChar());
  unGetc(readChar);
  BYTES_EQUAL('e', getChar());
  LONGS_EQUAL(EOF, getChar());
}
