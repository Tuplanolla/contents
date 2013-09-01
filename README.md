# Indefinix

Indefinix (In-def-i-nix) is a command line directory indexer.
Its purpose is to maintain a plain table of contents found in a directory.
The name is a distasteful mix of the words "index" and "definition", resulting in a word like "indefinite".

## Schedule

This project should be done by 2015.

## Motivation

Finding out what certain files or directories are for requires looking inside them, reading about them somewhere or asking someone.
Sometimes even that's not sufficient.
Using an index solves that problem.

## Installation

Indefinix is written in C and thus works on "any" system.

The following examples are from a (freshly installed) Arch Linux.

### Binaries

Binaries only need to be downloaded, extracted and moved to a suitable `$LOCATION`.

	[user@arch ~]$ wget https://github.com/Tuplanolla/indefinix/blob/master/indefinix.tar.gz
	[user@arch ~]$ tar -xf indefinix.tar.gz
	[user@arch ~]$ mv indefinix/bin/* $LOCATION

### Sources

Sources need to be downloaded and compiled.

Building Indefinix relies on
 the GNU Compiler Collection and
 GNU Make although
  any other C compiler and build automation tool should work as well.
GNU extensions are used, but not required.

	[user@arch ~]$ pacman -S gcc make

The libraries Indefinix depends on are
 the C standard library `libc` and
 a configuration file library `libconfig`.

	[user@arch ~]$ pacman -S libc libconfig

Acquiring Indefinix from GitHub also requires
 SSH and
 Git.

	[user@arch ~]$ pacman -S ssh git

Once the required packages are installed the repository can be cloned

	[user@arch ~]$ git clone git@github.com:Tuplanolla/indefinix.git

and Indefinix can be built.

	[user@arch ~]$ cd indefinix
	[user@arch indefinix]$ make

The binaries go in the `bin` directory and temporary objects in the `obj` directory.
After moving the binaries to a suitable `$LOCATION`

	[user@arch ~]$ mv indefinix/bin/* $LOCATION

the byproducts of the compilation can be removed.

	[user@arch indefinix]$ make clean

## Running

### Configuration

Indefinix can be configured persistently with the command `indefinix set (key) (value) (...)` or temporarily with optional flags in the short form `-(k) (value) (...)` or the long form `--(key) (value) (...)`.
Persistent configurations are saved to `~/.indefinix` if needed while temporary configurations are lost when the program exits.

The key `name`, abbreviated as `n`, contains the expected name of the index files.
The default value is `INDEX`.
Any file name is a valid `name`.

The key `editor`, abbreviated as `e`, contains the default text editor used for editing the index files with `indefinix edit` or the configuration file with `indefinix configure`.
The default value is empty unless a text editor is detected automatically.
Any path to an executable is a valid `editor`.

The key `completion`, abbreviated as `c`, contains the shortest command length that's allowed to be automatically completed.
The default value is `1`.
Automatic completion can be disabled with `0`, but `1` is recommended since every command has a unique initial and destructive actions have prompts.

The key `order`, abbreviated as `o`, contains the order used in the index file.
The default value is `normal,directories,hidden`.
Any string in the form `(sorting) (grouping) (hiding)` is a valid order string, where `(sorting)` is either `normal`, `reverse` or `none`, `(grouping)` is either `directories`, `files` or `none` and `(hiding)` is either `hidden` or `none`.

The key `wrapping`, abbreviated as `w`, contains the way to process long lines.
The default value is `wrap`.
Either of the values `wrap` or `none` is a valid `wrapping`.

The key `alignment`, abbreviated as `a`, contains the text alignment to use in the columns of the index files.
The default value is `left left`.
Any `(position) (position)` pair is valid `alignment`, where `(position)` is either `left`, `right` or `center`.

The key `filling`, abbreviated as `f`, contains how the columns of the index files are padded.
The default value is `fill fill`.
Any `(padding) (padding)` pair is valid `filling`, where `(padding)` is either `fill` or `none`.

The key `infix`, abbreviated as `i`, contains the text that comes between the columns.
The default value is `"   "` (three spaces).

Similarly the keys `prefix`, abbreviated as `p`, and `suffix`, abbreviated as `s`, contain the texts that come before the first column and after the last column respectively.
The default values are both `""`.

Furthermore the keys `headinfix`, abbreviated as `hi`; `headprefix`, abbreviated as `hp`, and `headsuffix`, abbreviated as `hs`, contain the corresponding texts on the first line only.
The default values are empty except for `" - "` for `headinfix`.

Finally the keys `tailinfix`, abbreviated as `ti`; `tailprefix`, abbreviated as `tp`, and `tailsuffix`, abbreviated as `ts`, contain the corresponding texts on the last line only.
The default values are empty.

The key `unusual`, abbreviated as `u`, contains the texts that are displayed in unusual situations.
The default value is `"not indexed" "not present"`.

Note that changing the affixes unexpectedly may confuse the parser.

The character `%` is equivalent to the default value of a key unless it's followed by another `%` and interpreted as a literal percent sign.

### Usage

A brief usage reference can be printed by calling Indefinix without any arguments.

	[user@arch /tmp]$ indefinix

Indefinix can be used with the commands `indefinix (flags) (command) (argument) (...) (flags)`, where the `(flags)` are optional and the `(command)` can be partial if it's unambiguous.
The order of commands and flags matters as it determines their order of application.

The flags are explained in the previous section and the commands in the following subsections.

#### Configuration

The command `configure` opens the persistent configuration in the default text editor.

The command `set (key) (value)` changes the persistent configuration by associating the given `(key)` with the given `(value)`.

The command `pop (key) (...)` changes the persistent configuration by removing the given keys.

The command `get (key) (...)` looks up the given keys in the persistent configuration.

The command `obliterate` deletes the persistent configuration.

#### Indexing

The command `make` creates a new index file.

The command `edit` opens the index file in the default text editor.

The command `add (entry) (description) (...)` adds the given `(entry)` to the index file with the given descriptions.

The command `remove (entry) (...)` removes the given entries from the index file.

The command `update (entry) (description) (...)` changes the given `(entry)` in the index file to have the given descriptions.

The command `lookup (entry) (...)` looks up the given entries in the index file or lists them all.

The command `find (string) (...)` searches the index file for the given strings.

The command `touch` rebuilds the index file if it's mangled.

The command `destroy` deletes the index file.

#### Example

Let's begin by configuring Indefinix for a Lisp fanatic.

	[user@arch ~]$ indefinix configure order none directories hidden
	[user@arch ~]$ indefinix configure alignment left left
	[user@arch ~]$ indefinix configure filling none none
	[user@arch ~]$ indefinix configure infix " ("
	[user@arch ~]$ indefinix configure prefix " "
	[user@arch ~]$ indefinix configure headprefix "("
	[user@arch ~]$ indefinix configure tailsuffix "))"

We'll then move to the directory we want to index

	[user@arch ~]$ cd /tmp

and create some dummy files and directories.

	[user@arch /tmp]$ touch README LICENSE
	[user@arch /tmp]$ mkdir documents music pictures

We can now create an index file and add some entries to it.

	[user@arch /tmp]$ indefinix make
	Created a new index "INDEX".
	[user@arch /tmp]$ indefinix add music performed scores
	Added a new entry "music".
	[user@arch /tmp]$ indefinix add documents "books, papers and other text documents"
	Added a new entry "documents".
	[user@arch /tmp]$ indefinix add LICENSE legal "nonsense"
	Added a new entry "LICENSE".

Let's see how it looks.

	[user@arch /tmp]$ indefinix lookup
	(music/ (performed scores))
	(documents/ (books, papers and
	 other text documents))
	(LICENSE (legal nonsense))

It looks like a mess, so it's best to reset the configuration.
We also have to rebuild the index since the configuration changes.

	[user@arch /tmp]$ indefinix touch -o % -a % -f % -i % -p % -hp % -ts %
	[user@arch /tmp]$ indefinix obliterate
	Do you really want to delete the configuration file "/home/user/.indefinix"? (y / N) y
	Obliterated the persistent configuration.

Let's take a look at it again, but only at specific things.

	[user@arch /tmp]$ indefinix lookup pictures documents LICENSE nothing/ more
	pictures   - not indexed
	documents/ - books, papers and
	             other text documents
	LICENSE    - legal nonsense
	nothing/   - not present
	more       - not present

It's looking good, but the description of music needs to be more accurate.

	[user@arch /tmp]$ indefinix update music recorded pressure waves
	Updated the entry "music".

Now it's easier to find.

	[user@arch /tmp]$ indefinix find wave
	music - recorded pressure waves

To wrap it up, let's remove an entry

	[user@arch /tmp]$ indefinix remove documents
	Removed the entry "documents".

and destroy the index.

	[user@arch /tmp]$ indefinix destroy
	Do you really want to delete the index file "/tmp/INDEX"? (y / N) y
	Destroyed the index.

Now the index can't be used anymore.

	[user@arch /tmp]$ indefinix find meaning
	Failed to find the index "INDEX".
	[user@arch /tmp]$ echo $?
	1

#### Integration

It's a good idea to add Indefinix to the lookup `$PATH` and give it a shorter alias

	[user@arch ~]$ which indefinix
	[user@arch ~]$ alias ind=indefinix

to make life easier.

	[user@arch ~]$ ind l

## Development

### Directory Structure

Files are named and organized in a typical manner.
The directories are

* `/` for the most important files,
* `/src` for sources,
* `/obj` for temporary build files,
* `/lib` for libraries,
* `/bin` for binaries,
* `/doc` for documentation and
* `/etc` for other files.
