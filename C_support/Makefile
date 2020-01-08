#This makefile makes all the main book code with CppUTest test harness

#Set this to @ to keep the makefile quiet
SILENCE = @

#---- Outputs ----#
COMPONENT_NAME = SchemeInC

#--- Inputs ----#
PROJECT_HOME_DIR = .

SRC_DIRS = src

BIN_SRC = $(wildcard bin/**/*.c bin/*.c)
BINS= $(patsubst %.c,%,$(BIN_SRC))

TEST_SRC_DIRS = tests

MOCKS_SRC_DIRS = mocks

INCLUDE_DIRS = include\
	$(CPPUTEST_HOME)/include

CPPUTEST_WARNINGFLAGS=-Wall

# for deploy
# CPPUTEST_USE_MEM_LEAK_DETECTION=N

include $(CPPUTEST_HOME)/build/MakefileWorker.mk

bin: CFLAGS=-Wall lib/lib$(COMPONENT_NAME).a -Iinclude
bin: CPPFLAGS= # suppress
bin: $(BINS)

binClean: clean
	rm -f $(BINS)
