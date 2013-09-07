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
Currently the focus is Linux, but any other system should be trivial to support.

The following examples are from a (freshly installed) Arch Linux.

### Binaries

Binaries only need to be downloaded, extracted and moved to a suitable `$LOCATION`.

	[user@arch ~]$ wget https://github.com/Tuplanolla/indefinix/blob/master/pkg/indefinix.tar.gz
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
After automatically moving the binaries to `/usr/local/bin` or `/usr/bin`

	[user@arch indefinix]$ make install

or manually to a suitable `$LOCATION`,

	[user@arch indefinix]$ mv bin/* $LOCATION

the byproducts of the compilation can be removed.

	[user@arch indefinix]$ make clean

## Running

### Configuration

Indefinix can be configured persistently with the command `indefinix set (key) (values)` or temporarily with optional flags in the short form `-(k) (values)` or the long form `--(key) (values)`.
Persistent configurations are saved to `~/.indefinix` if needed while temporary configurations are lost when the program exits.

The key `location (name)`, abbreviated as `l`, contains the expected name of the index files.
The default value is `INDEX`.
Any file name is a valid `(name)`.

The key `editor (path)`, abbreviated as `e`, contains the default text editor used for editing the index files with `indefinix edit` or the configuration file with `indefinix configure`.
The default value is `%n` unless a simple text editor is detected automatically.
Any path to an executable is a valid `(path)`.

The key `completion (number)`, abbreviated as `c`, contains the shortest command length that can be automatically completed.
The default value is `1`.
Any unsigned integer is a valid `(number)`.
Automatic completion can be disabled with `0`, but `1` is recommended since every command has a unique initial and destructive actions require verification.

The key `order (sorting) (grouping) (hiding)`, abbreviated as `o`, contains the order used in the index file.
The default value is `normal directories hidden`.
Any of `normal`, `reverse` or `none` is a valid `(sorting)`; either `directories`, `files` or `none` is a valid `(grouping)` and either `hidden` or `none` is a valid `(hiding)`.

The key `wrapping (continuation)`, abbreviated as `w`, contains the way to process long lines.
The default value is `wrap`.
Any of `wrap` or `none` is a valid `(continuation)`.

The key `justification (alignment) (alignment)`, abbreviated as `j`, contains the text alignment to use in the columns of the index files.
The default value is `left left`.
Any of `left`, `right` or `center` is a valid `(alignment)`.

The key `filling (padding) (padding)`, abbreviated as `f`, contains how the columns of the index files are padded.
The default value is `fill fill`.
Any of `fill` or `none` is a valid `(padding)`.

The key `interaction (answer)`, abbreviated as `i` automatically answers interactive prompts if they come up.
The default value is `%n`.
Any of `yes` or `no` is a valid `(answer)`.

The key `affix (string) (string) (string)`, abbreviated as `a`, contains the text that comes before, between and after the columns of the index files.
The default value is `"   " "" ""` (the first part having three spaces).

Furthermore the keys `headaffix (string) (string) (string)`, abbreviated as `ha` and `tailaffix (string) (string) (string)`, abbreviated as `ta`, work like affix, but only apply to the first and last lines respectively.
The default values are `%n " - " %n` and `%n %n %n`.

Note that changing the affixes unexpectedly may confuse the parser.

The key `unusual (string) (string)`, abbreviated as `u`, contains the texts that are displayed in unusual situations.
The default value is `"not indexed" "not present"`.

Obviously any string is a valid `(string)`.

### Usage

Indefinix can be used with the command `indefinix (flags) (command) (arguments) (flags) (...)`, where the `(flags)` are optional and the `(command)` can be partial if it's unambiguous and automatic command completion is allowed.
The omitted part `(...)` can contain further commands and flags.
The order of commands and flags matters as it determines their order of application.

The flags are explained in the previous section and the commands in the following subsections.

#### Configuring

The command `configure` opens the persistent configuration file in the default text editor.

The command `set (key) (values)` changes the persistent configuration by associating the given `(key)` with the given `(values)`.

The command `pop (key)` changes the persistent configuration by removing the given `(key)`.

The command `get (key)` looks up the given `(key)` in the persistent configuration.

The command `obliterate` deletes the persistent configuration.

#### Indexing

The command `make (template)` creates a new index file based on the given `(template)`.

The command `edit` opens the index file in the default text editor.

The command `add (entry) (description)` adds the given `(entry)` to the index file with the given `(description)`.

The command `remove (entry)` removes the given `(entry)` from the index file.

The command `update (entry) (description)` changes the given `(entry)` in the index file to have the given `(description)`.

The command `lookup (entry)` looks up the given `(entry)` in the index file or lists them all.

The command `find (string)` searches the index file for the given `(string)`.

The command `touch` rebuilds the index file if it's mangled.

The command `destroy` deletes the index file.

#### General

The command `help` prints a short usage reference, which also appears when Indefinix is invoked without any arguments.

The command `version` prints a short summary, including version information.

#### Special Cases

In addition to configuration keys and index entries, there are a handful of special cases.

The special key `preset (selection)`, abbreviated as `p`, is equivalent to every key simultaneously.
The default value is, rather self referentially, `default`.
The key doesn't have a persistent value, so it can only be written.
Only `default` is a valid `(selection)`.

The special value `%` is equivalent to an empty (essentially `NULL`) value and different from `""`.
The string `%%` works as a literal `%` since `%` alone is reserved.

The special command `bind (command) (arguments)` gives all the subsequent arguments to the next command, flags included.
The two following commands are identical for example.

	[user@arch ~]$ indefinix bind lookup pictures videos --unusual "minor error" "major error"
	[user@arch ~]$ indefinix lookup pictures lookup videos lookup --unusual lookup "minor error" lookup "major error"

#### Example

Let's begin by configuring Indefinix for a Lisp fanatic.

	[user@arch ~]$ indefinix configure order none directories hidden
	[user@arch ~]$ indefinix configure alignment left left
	[user@arch ~]$ indefinix configure filling none none
	[user@arch ~]$ indefinix configure affix " " " (" ""
	[user@arch ~]$ indefinix configure headaffix "(" % %
	[user@arch ~]$ indefinix configure tailaffix % % "))"

We'll then move to the directory we want to index

	[user@arch ~]$ cd /tmp

and create some dummy files and directories.

	[user@arch /tmp]$ touch README LICENSE
	[user@arch /tmp]$ mkdir documents music pictures

We can now create an index file and add some entries to it.

	[user@arch /tmp]$ indefinix make empty
	Created a new index "INDEX".
	[user@arch /tmp]$ indefinix add music "performed scores"
	Added a new entry "music".
	[user@arch /tmp]$ indefinix add documents "books, papers and other text documents"
	Added a new entry "documents".
	[user@arch /tmp]$ indefinix add LICENSE "legal nonsense"
	Added a new entry "LICENSE".

Let's see how it looks.

	[user@arch /tmp]$ indefinix lookup
	(music/ (performed scores))
	(documents/ (books, papers and
	 other text documents))
	(LICENSE (legal nonsense))

It looks like a mess, so it's best to reset the configuration.
We also have to rebuild the index to avoid confusing the parser later.

	[user@arch /tmp]$ indefinix touch configure preset default

The configuration could also be removed instead.

	[user@arch /tmp]$ indefinix touch --preset default
	[user@arch /tmp]$ indefinix obliterate
	Do you really want to delete the configuration file "/home/user/.indefinix"? (y / N) y
	Obliterated the persistent configuration.

Let's take a look at the index again, but only at specific things.

	[user@arch /tmp]$ indefinix bind lookup pictures documents LICENSE nothing/ more
	pictures   - not indexed
	documents/ - books, papers and
	             other text documents
	LICENSE    - legal nonsense
	nothing/   - not present
	more       - not present

It's looking good, but the description of music needs to be more accurate.

	[user@arch /tmp]$ indefinix update music "recorded pressure waves"
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
* `/pkg` for packages,
* `/dox` for automatic documentation and
* `/etc` for other files.
