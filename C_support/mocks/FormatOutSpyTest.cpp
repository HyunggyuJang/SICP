extern "C"
{
#include "FormatOutSpy.h"
#include "../include/formatOut.h"
#include <stdio.h>
}

#include "CppUTest/TestHarness.h"

TEST_GROUP(FormatOutSpy)
{
  char *stdoutBuf = NULL;
  char *stderrBuf = NULL;
    void setup()
    {
      CHECK(FormatOutSpy_Create(100));
      CHECK(stdoutBuf = FormatOutSpy_GetOutput(stdout));
      CHECK(stderrBuf = FormatOutSpy_GetOutput(stderr));
    }

    void teardown()
    {
       FormatOutSpy_Destroy();
    }
};

TEST(FormatOutSpy, CanRetriveStdout)
{
  char expected[] = "Blah Blah Blash";
  LONGS_EQUAL(sizeof expected - 1, formatOut(stdout, "Blah Blah %s", "Blash"));
  STRCMP_EQUAL(expected, stdoutBuf);
  STRCMP_EQUAL("", stderrBuf);
}

TEST(FormatOutSpy, CannotWriteOver)
{
  FormatOutSpy_Destroy();
  FormatOutSpy_Create(10);
  char expected[] = "Blah Blah ";
  LONGS_EQUAL(sizeof expected - 1,
              formatOut(stdout, "Blah Blah %s", "Blash"));
  STRCMP_EQUAL(expected,
               FormatOutSpy_GetOutput(stdout));
}

TEST(FormatOutSpy, CanRetriveStderr)
{
  char expected[] = "Error! TEST!";
  LONGS_EQUAL(sizeof expected - 1, formatOut(stderr, "Error! %s", "TEST!"));
  STRCMP_EQUAL("", stdoutBuf);
  STRCMP_EQUAL(expected, stderrBuf);
}
