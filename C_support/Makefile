#This makefile makes all the main book code with CppUTest test harness

#Set this to @ to keep the makefile quiet
SILENCE = @

#---- Outputs ----#
COMPONENT_NAME = SchemeInC

#--- Inputs ----#
PROJECT_HOME_DIR = .

SRC_DIRS = src

TEST_SRC_DIRS = tests

MOCKS_SRC_DIRS = mocks

INCLUDE_DIRS = include\
	$(CPPUTEST_HOME)/include

CPPUTEST_WARNINGFLAGS=-Wall

include $(CPPUTEST_HOME)/build/MakefileWorker.mk

