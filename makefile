# Anything should fit on eight pages.
SIZE = 32766
# System headers trigger some warnings.
WEVERYTHING = -Waddress -Waggregate-return -Wall -Warray-bounds -Wcast-align -Wcast-qual -Wchar-subscripts -Wclobbered -Wcomment -Wconversion -Wcoverage-mismatch -Wdisabled-optimization -Wempty-body -Wenum-compare -Wextra -Wfloat-equal -Wformat -Wformat-nonliteral -Wformat-security -Wformat-y2k -Wformat=2 -Wframe-larger-than=$(SIZE) -Wignored-qualifiers -Wimplicit -Wimplicit-function-declaration -Wimplicit-int -Winit-self -Winline -Winvalid-pch -Wlarger-than=$(SIZE) -Wlogical-op -Wmain -Wmissing-braces -Wmissing-field-initializers -Wmissing-format-attribute -Wmissing-include-dirs -Wmissing-noreturn -Wno-attributes -Wno-builtin-macro-redefined -Wno-deprecated -Wno-deprecated-declarations -Wno-div-by-zero -Wno-endif-labels -Wno-format-contains-nul -Wno-format-extra-args -Wno-int-to-pointer-cast -Wno-mudflap -Wno-multichar -Wno-overflow -Wno-pointer-to-int-cast -Wno-pragmas -Wnonnull -Woverlength-strings -Wpacked -Wpacked-bitfield-compat -Wparentheses -Wpointer-arith -Wreturn-type -Wsequence-point -Wshadow -Wsign-compare -Wsign-conversion -Wstack-protector -Wstrict-aliasing -Wstrict-overflow=5 -Wswitch -Wswitch-default -Wswitch-enum -Wsync-nand -Wsystem-headers -Wtrigraphs -Wtype-limits -Wundef -Wuninitialized -Wunknown-pragmas -Wunreachable-code -Wunsafe-loop-optimizations -Wunused -Wunused-function -Wunused-label -Wunused-parameter -Wunused-value -Wunused-variable -Wvariadic-macros -Wvla -Wvolatile-register-var -Wwrite-strings
#		-Wlong-long -Wpadded -Wredundant-decls
# Clang has -Weverything while GCC doesn't.
DEBUG = -g\
		-save-temps=obj -masm=intel -fverbose-asm\
		-O0\
		$(WEVERYTHING)
RELEASE = -s\
		-O3\
		-Wall -Wextra
# CHEAT needs the project root to be in the include path.
FLAGS = -std=c11\
		-D_POSIX_C_SOURCE=200112L -D_XOPEN_SOURCE=600\
		$(DEBUG)\
		-I .
CC = /usr/bin/gcc $(FLAGS)
MKDIR = /bin/mkdir -p
RM = /bin/rm -f
CP = /bin/cp -u
TAR = /bin/tar -c
DOXYGEN = /usr/bin/doxygen
PRIMARY = /usr/local/bin
SECONDARY = /usr/bin
SRC = src
OBJ = obj
BIN = bin
PKG = pkg
DOX = dox
NAME = indefinix
MAIN_SOURCES = $(SRC)/main.c
TEST_SOURCES = $(SRC)/cheat.c
# find src -name "*.c" -type f | sort | sed -e "s/src\//\$(SRC)\//" | xargs echo
SOURCES = $(SRC)/action.c $(SRC)/arity.c $(SRC)/array.c $(SRC)/calculator.c $(SRC)/data.c $(SRC)/indefinix.c $(SRC)/property.c $(SRC)/resolver.c $(SRC)/state.c
MAIN_OBJECTS = $(MAIN_SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
TEST_OBJECTS = $(TEST_SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
OBJECTS = $(SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
BINARY = $(BIN)/$(NAME)
SUITE = $(BIN)/test-$(NAME)

all: build

run: build
	$(BINARY) $(ARGUMENTS)

test: harness
	$(SUITE) $(ARGUMENTS)

package: build
	$(TAR) -f $(PKG)/$(NAME).tar.gz -C .. $(NAME)/$(BINARY)

document: $(SOURCES)
	$(DOXYGEN)

install: build
	$(CP) $(BINARY) $(PRIMARY)/$(NAME) || $(CP) $(BINARY) $(SECONDARY)/$(NAME)

uninstall:
	$(RM) $(PRIMARY)/$(BINARY) && $(RM) $(SECONDARY)/$(NAME)

wipe:
	$(RM) $(HOME)/.$(NAME)

build: $(SOURCES) $(MAIN_SOURCES) prepare $(BINARY)

harness: $(SOURCES) $(TEST_SOURCES) prepare $(SUITE)

prepare:
	$(MKDIR) $(OBJ)
	$(MKDIR) $(BIN)
	$(MKDIR) $(PKG)
	$(MKDIR) $(DOX)

clean:
	$(RM) -r $(OBJ)
	$(RM) -r $(BIN)
	$(RM) -r $(PKG)
	$(RM) -r $(DOX)

$(BINARY): $(MAIN_OBJECTS) $(OBJECTS)
	$(CC) -o $@ $^

$(SUITE): $(TEST_OBJECTS) $(OBJECTS)
	$(CC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.c
	$(CC) -c -o $@ $<

# Abstract build targets aren't files.
.PHONY: all run test package document install uninstall wipe build harness prepare clean
