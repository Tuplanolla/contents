RM = /bin/rm -f -r
MKDIR = /bin/mkdir -p
SIZE = 32766
DEBUG = -g -save-temps=obj -fverbose-asm -masm=intel\
		-O0\
		-Waddress -Waggregate-return -Wall -Warray-bounds -Wcast-align -Wcast-qual -Wchar-subscripts -Wclobbered -Wcomment -Wconversion -Wcoverage-mismatch -Wdisabled-optimization -Wempty-body -Wenum-compare -Wextra -Wfloat-equal -Wformat -Wformat-nonliteral -Wformat-security -Wformat-y2k -Wformat=2 -Wframe-larger-than=$(SIZE) -Wignored-qualifiers -Wimplicit -Wimplicit-function-declaration -Wimplicit-int -Winit-self -Winline -Winvalid-pch -Wlarger-than=$(SIZE) -Wlogical-op -Wlong-long -Wmain -Wmissing-braces -Wmissing-field-initializers -Wmissing-format-attribute -Wmissing-include-dirs -Wmissing-noreturn -Wno-attributes -Wno-builtin-macro-redefined -Wno-deprecated -Wno-deprecated-declarations -Wno-div-by-zero -Wno-endif-labels -Wno-format-contains-nul -Wno-format-extra-args -Wno-int-to-pointer-cast -Wno-mudflap -Wno-multichar -Wno-overflow -Wno-pointer-to-int-cast -Wno-pragmas -Wnonnull -Woverlength-strings -Wpacked -Wpacked-bitfield-compat -Wpadded -Wparentheses -Wpointer-arith -Wredundant-decls -Wreturn-type -Wsequence-point -Wshadow -Wsign-compare -Wsign-conversion -Wstack-protector -Wstrict-aliasing -Wstrict-overflow=5 -Wswitch -Wswitch-default -Wswitch-enum -Wsync-nand -Wsystem-headers -Wtrigraphs -Wtype-limits -Wundef -Wuninitialized -Wunknown-pragmas -Wunreachable-code -Wunsafe-loop-optimizations -Wunused -Wunused-function -Wunused-label -Wunused-parameter -Wunused-value -Wunused-variable -Wvariadic-macros -Wvla -Wvolatile-register-var -Wwrite-strings
RELEASE = -s\
		-O3\
		-Wall -Wextra
FLAGS = -std=c11\
		$(DEBUG)\
#		-lm -lconfig -letc
CC = /usr/bin/gcc $(FLAGS)
SRC = src
OBJ = obj
BIN = bin
# find src -name "*.c" -type f | sort | sed -e "s/src\//\$(SRC)\//" | xargs echo
SOURCES = $(SRC)/data.c $(SRC)/helper.c $(SRC)/main.c $(SRC)/parser.c $(SRC)/project.c $(SRC)/resolver.c
OBJECTS = $(SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
BINARY = $(BIN)/indefinix

all: build

run: build
	$(BINARY) $(ARGUMENTS)

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
