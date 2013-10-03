# Anything should fit on eight pages.
SIZE = 32766
# GCC version 4.7.3 has lots of warning options.
WEVERYTHING = -Wabi -Waddress -Waggregate-return -Wall -Warray-bounds -Wattributes -Wbad-function-cast -Wbuiltin-macro-redefined -Wcast-align -Wchar-subscripts -Wclobbered -Wcomment -Wcomments -Wconversion -Wcoverage-mismatch -Wcpp -Wdeprecated -Wdeprecated-declarations -Wdisabled-optimization -Wdiv-by-zero -Wdouble-promotion -Wempty-body -Wendif-labels -Wenum-compare -Wextra -Wfloat-equal -Wformat-contains-nul -Wformat-extra-args -Wformat-nonliteral -Wformat-security -Wformat-y2k -Wformat-zero-length -Wformat=2 -Wframe-larger-than=$(SIZE) -Wfree-nonheap-object -Wignored-qualifiers -Wimplicit -Wimplicit-function-declaration -Wimplicit-int -Winit-self -Winline -Wint-to-pointer-cast -Winvalid-memory-model -Winvalid-pch -Wjump-misses-init -Wlarger-than=$(SIZE) -Wlogical-op -Wlong-long -Wmain -Wmaybe-uninitialized -Wmissing-braces -Wmissing-declarations -Wmissing-field-initializers -Wmissing-format-attribute -Wmissing-include-dirs -Wmissing-noreturn -Wmissing-parameter-type -Wmissing-prototypes -Wmudflap -Wmultichar -Wnarrowing -Wnested-externs -Wno-attributes -Wno-builtin-macro-redefined -Wno-cpp -Wno-deprecated -Wno-deprecated-declarations -Wno-div-by-zero -Wno-endif-labels -Wno-format-contains-nul -Wno-format-extra-args -Wno-free-nonheap-object -Wno-int-to-pointer-cast -Wno-mudflap -Wno-multichar -Wno-overflow -Wno-pointer-to-int-cast -Wno-pragmas -Wno-unused-result -Wnonnull -Wnormalized=nfkc -Wold-style-declaration -Wold-style-definition -Woverflow -Woverlength-strings -Woverride-init -Wpacked -Wpacked-bitfield-compat -Wparentheses -Wpointer-arith -Wpointer-sign -Wpointer-to-int-cast -Wpragmas -Wredundant-decls -Wreturn-type -Wsequence-point -Wshadow -Wsign-compare -Wsign-conversion -Wstack-protector -Wstack-usage=$(SIZE) -Wstrict-aliasing=3 -Wstrict-overflow=5 -Wstrict-prototypes -Wsuggest-attribute=const -Wsuggest-attribute=noreturn -Wsuggest-attribute=pure -Wswitch -Wswitch-default -Wswitch-enum -Wsync-nand -Wtrampolines -Wtrigraphs -Wtype-limits -Wundef -Wuninitialized -Wunknown-pragmas -Wunreachable-code -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wunused -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wunused-local-typedefs -Wunused-macros -Wunused-parameter -Wunused-result -Wunused-value -Wunused-variable -Wvariadic-macros -Wvector-operation-performance -Wvla -Wvolatile-register-var -Wwrite-strings -pedantic
#		-Wcast-qual -Wdeclaration-after-statement -Wpadded -Wsystem-headers -Wtraditional -Wtraditional-conversion
# Clang has -Weverything while GCC doesn't.
DEBUG_CFLAGS = -g\
		-save-temps=obj -masm=intel -fverbose-asm\
		-O0\
		$(WEVERYTHING)
RELEASE_CFLAGS = -s\
		-O3\
		-Wall -Wextra
# CHEAT needs the project root to be in the include path.
TEST_CFLAGS = -D_POSIX_C_SOURCE=200809L -D_XOPEN_SOURCE=700\
		-I .
CFLAGS = -std=c11\
		$(DEBUG_CFLAGS)\
		$(TEST_CFLAGS)
CC = /usr/bin/gcc
RM = /bin/rm -f
CP = /bin/cp -u
AR = /bin/tar -c
SH = /bin/sh
MKDIR = /bin/mkdir -p
ECHO = /bin/echo
LESS = /bin/less
SED = /bin/sed
DOXYGEN = /usr/bin/doxygen
PRIMARY = /usr/local/bin
SECONDARY = /usr/bin
SRC = src
OBJ = obj
BIN = bin
PKG = pkg
DOX = dox
# pwd | xargs basename
NAME = indefinix
MAIN_SOURCES = $(SRC)/indefinix.c
TEST_SOURCES = $(SRC)/test-array.c $(SRC)/test-cheat.c
# find src -name "*.c" -type f | sort | sed "s/src\//\$(SRC)\//" | xargs echo
SOURCES = $(SRC)/bindefinix.c $(SRC)/action.c $(SRC)/actions.c $(SRC)/arity.c $(SRC)/array.c $(SRC)/calculator.c $(SRC)/executor.c $(SRC)/helper.c $(SRC)/invocation.c $(SRC)/project.c $(SRC)/properties.c $(SRC)/property.c $(SRC)/resolution.c $(SRC)/state.c $(SRC)/suggestion.c $(SRC)/suggestions.c $(SRC)/tracer.c $(SRC)/truncation.c
OBJECTS = $(SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
MAIN_BINARIES = $(MAIN_SOURCES:$(SRC)/%.c=$(BIN)/%)
TEST_BINARIES = $(TEST_SOURCES:$(SRC)/%.c=$(BIN)/%)
DOCUMENTATION = README.md LICENSE.txt

all: build

help: $(DOCUMENTATION)
	$(LESS) $(DOCUMENTATION)

run: $(MAIN_BINARIES)
	$(ECHO) $(MAIN_BINARIES) | $(SED) "s/ \\|$$/ $(ARGUMENTS)\\n/g" | $(SH)

test: $(TEST_BINARIES)
	$(ECHO) $(TEST_BINARIES) | $(SED) "s/ \\|$$/ $(ARGUMENTS)\\n/g" | $(SH)

package: build
	$(AR) -f $(PKG)/$(NAME).tar.gz -C .. $(MAIN_BINARIES:%=$(NAME)/%) $(DOCUMENTATION:%=$(NAME)/%)

document: $(SOURCES)
	$(DOXYGEN)

install: build
	$(CP) $(MAIN_BINARIES) $(PRIMARY) || $(CP) $(MAIN_BINARIES) $(SECONDARY)

uninstall:
	$(RM) $(PRIMARY)/$(NAME) && $(RM) $(SECONDARY)/$(NAME)

wipe:
	$(RM) $(HOME)/.$(NAME)

build: $(SOURCES) $(MAIN_SOURCES) mess $(MAIN_BINARIES)

harness: $(SOURCES) $(TEST_SOURCES) mess $(TEST_BINARIES)

mess:
	$(MKDIR) $(OBJ)
	$(MKDIR) $(BIN)
	$(MKDIR) $(PKG)
	$(MKDIR) $(DOX)

clean:
	$(RM) -r $(OBJ)
	$(RM) -r $(BIN)
	$(RM) -r $(PKG)
	$(RM) -r $(DOX)

# Compiler invocations are long.
$(BIN)/%: $(OBJECTS) $(OBJ)/%.o
	@ $(ECHO) $(CC) ... -o $@ $^
	@ $(CC) $(CFLAGS) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.c
	@ $(ECHO) $(CC) ... -c -o $@ $<
	@ $(CC) $(CFLAGS) -c -o $@ $<

# Abstract build targets aren't files.
.PHONY: all help run test package document install uninstall wipe build harness mess clean
