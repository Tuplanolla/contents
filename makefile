RM = /bin/rm -f -r
MKDIR = /bin/mkdir -p
CC = /usr/bin/gcc -std=c11\
		-g -save-temps=obj -masm=intel -fverbose-asm\
		-Wall -Wextra
#		-O3\
#		-lconfig\

SRC = src
OBJ = obj
BIN = bin
SOURCES = $(SRC)/main.c# $(SRC)/asm.c
OBJECTS = $(OBJ)/main.o
BINARY = $(BIN)/indefinix

all: build

run: build
	$(EXECUTABLE)

build: $(SOURCES) prepare $(BINARY)

prepare:
	$(MKDIR) $(OBJ)
	$(MKDIR) $(BIN)

$(BINARY): $(OBJECTS)
	$(CC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.c
	$(CC) -c -o $@ $<

clean:
	$(RM) $(OBJ)
	$(RM) $(BIN)

.PHONY: all run clean prepare
