# Anything should fit on eight pages.
SIZE = 32766
# GCC version 4.7.3 has lots of warning options.
WEVERYTHING = -Wabi -Waddress -Waggregate-return -Wall -Warray-bounds -Wattributes -Wbad-function-cast -Wbuiltin-macro-redefined -Wcast-align -Wcast-qual -Wchar-subscripts -Wclobbered -Wcomment -Wcomments -Wconversion -Wcoverage-mismatch -Wcpp -Wdeprecated -Wdeprecated-declarations -Wdisabled-optimization -Wdiv-by-zero -Wdouble-promotion -Wempty-body -Wendif-labels -Wenum-compare -Wextra -Wfloat-equal -Wformat-contains-nul -Wformat-extra-args -Wformat-nonliteral -Wformat-security -Wformat-y2k -Wformat-zero-length -Wformat=2 -Wframe-larger-than=$(SIZE) -Wfree-nonheap-object -Wignored-qualifiers -Wimplicit -Wimplicit-function-declaration -Wimplicit-int -Winit-self -Winline -Wint-to-pointer-cast -Winvalid-memory-model -Winvalid-pch -Wjump-misses-init -Wlarger-than=$(SIZE) -Wlogical-op -Wlong-long -Wmain -Wmaybe-uninitialized -Wmissing-braces -Wmissing-declarations -Wmissing-field-initializers -Wmissing-format-attribute -Wmissing-include-dirs -Wmissing-noreturn -Wmissing-parameter-type -Wmissing-prototypes -Wmudflap -Wmultichar -Wnarrowing -Wnested-externs -Wno-attributes -Wno-builtin-macro-redefined -Wno-cpp -Wno-deprecated -Wno-deprecated-declarations -Wno-div-by-zero -Wno-endif-labels -Wno-format-contains-nul -Wno-format-extra-args -Wno-free-nonheap-object -Wno-int-to-pointer-cast -Wno-mudflap -Wno-multichar -Wno-overflow -Wno-pointer-to-int-cast -Wno-pragmas -Wno-unused-result -Wnonnull -Wnormalized=nfkc -Wold-style-declaration -Wold-style-definition -Woverflow -Woverlength-strings -Woverride-init -Wpacked -Wpacked-bitfield-compat -Wpadded -Wparentheses -Wpointer-arith -Wpointer-sign -Wpointer-to-int-cast -Wpragmas -Wredundant-decls -Wreturn-type -Wsequence-point -Wshadow -Wsign-compare -Wsign-conversion -Wstack-protector -Wstack-usage=$(SIZE) -Wstrict-aliasing=3 -Wstrict-overflow=5 -Wstrict-prototypes -Wsuggest-attribute=const -Wsuggest-attribute=noreturn -Wsuggest-attribute=pure -Wswitch -Wswitch-default -Wswitch-enum -Wsync-nand -Wtrampolines -Wtrigraphs -Wtype-limits -Wundef -Wuninitialized -Wunknown-pragmas -Wunreachable-code -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wunused -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wunused-local-typedefs -Wunused-macros -Wunused-parameter -Wunused-result -Wunused-value -Wunused-variable -Wvariadic-macros -Wvector-operation-performance -Wvla -Wvolatile-register-var -Wwrite-strings -pedantic
#		-Wdeclaration-after-statement -Wsystem-headers -Wtraditional -Wtraditional-conversion
# Clang has -Weverything while GCC doesn't.
DEBUG = -g\
		-save-temps=obj -masm=intel -fverbose-asm\
		-O0\
		$(WEVERYTHING)
RELEASE = -s\
		-O3\
		-Wall -Wextra
# CHEAT needs the project root to be in the include path.
TEST = -D_POSIX_C_SOURCE=200809L -D_XOPEN_SOURCE=700\
		-I .
FLAGS = -std=c11\
		$(DEBUG)\
		$(TEST)
# Compiler invocations are long.
CC = /usr/bin/gcc $(FLAGS)
ECHO = /bin/echo
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
# pwd | xargs basename
DIRECTORY = indefinix
MAIN_SOURCES = $(SRC)/main.c
TEST_SOURCES = $(SRC)/cheat.c
# find src -name "*.c" -type f | sort | sed "s/src\//\$(SRC)\//" | xargs echo
SOURCES = $(SRC)/indefinix.c
MAIN_OBJECTS = $(MAIN_SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
TEST_OBJECTS = $(TEST_SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
OBJECTS = $(SOURCES:$(SRC)/%.c=$(OBJ)/%.o)
BINARY = $(BIN)/$(NAME)
SUITE = $(BIN)/test-$(NAME)
README = README.md

all: build

run: .run build
	@ $(BINARY) $(ARGUMENTS)
	@ $(ECHO) "...done!"

.run:
	@ $(ECHO) "Running the executable..."

test: .test harness
	@ $(SUITE) $(ARGUMENTS)
	@ $(ECHO) "...done!"

.test:
	@ $(ECHO) "Running tests..."

package: build
	@ $(ECHO) "Packaging everything..."
	@ $(TAR) -f $(PKG)/$(NAME).tar.gz -C .. $(DIRECTORY)/$(BINARY) $(DIRECTORY)/$(README)
	@ $(ECHO) "...done!"

document: $(SOURCES)
	@ $(ECHO) "Generating documentation..."
	@ $(DOXYGEN)
	@ $(ECHO) "...done!"

install: build
	@ $(ECHO) "Copying executables..."
	@ $(CP) $(BINARY) $(PRIMARY)/$(NAME) || $(CP) $(BINARY) $(SECONDARY)/$(NAME)
	@ $(ECHO) "...done!"

uninstall:
	@ $(ECHO) "Removing executables..."
	@ $(RM) $(PRIMARY)/$(BINARY) && $(RM) $(SECONDARY)/$(NAME)
	@ $(ECHO) "...done!"

wipe:
	@ $(ECHO) "Removing configurations..."
	@ $(RM) $(HOME)/.$(NAME)
	@ $(ECHO) "...done!"

build: $(SOURCES) $(MAIN_SOURCES) prepare .build $(BINARY)
	@ $(ECHO) "...done!"

.build:
	@ $(ECHO) "Building the executable..."

harness: $(SOURCES) $(TEST_SOURCES) prepare .harness $(SUITE)
	@ $(ECHO) "...done!"

.harness:
	@ $(ECHO) "Building tests..."

prepare:
	@ $(ECHO) "Making target directories..."
	@ $(MKDIR) $(OBJ)
	@ $(MKDIR) $(BIN)
	@ $(MKDIR) $(PKG)
	@ $(MKDIR) $(DOX)
	@ $(ECHO) "...done!"

clean:
	@ $(ECHO) "Removing target directories..."
	@ $(RM) -r $(OBJ)
	@ $(RM) -r $(BIN)
	@ $(RM) -r $(PKG)
	@ $(RM) -r $(DOX)
	@ $(ECHO) "...done!"

$(BINARY): $(MAIN_OBJECTS) $(OBJECTS)
	@ $(CC) -o $@ $^

$(SUITE): $(TEST_OBJECTS) $(OBJECTS)
	@ $(CC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.c
	@ $(CC) -c -o $@ $<

# Abstract build targets aren't files.
.PHONY: all run test package document install uninstall wipe build harness prepare clean
