CFLAGS = -O -Wall -Isrc -I../include

SOURCES=$(wildcard src/**/*.c src/*.c)
OBJECTS=$(patsubst %.c,%.o,$(SOURCES))
COMPILED_SRC = $(wildcard compiled/**/*.c compiled/*.c)
COMPILED = $(patsubst %.c,%,$(COMPILED_SRC))

TARGET=build/libSICP.a

all: $(TARGET) compile

$(TARGET): build $(OBJECTS)
	ar rcs $@ $(OBJECTS)
	ranlib $@

build:
	@mkdir -p build

compile: CFLAGS += $(TARGET) -DNDEBUG
compile: $(COMPILED)

clean:
	rm -rf build $(OBJECTS) $(COMPILED)
	rm -rf $(COMPILED_SRC)
