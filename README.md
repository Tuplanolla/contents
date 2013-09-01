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

All the names are subject to change.

### Configuration

Indefinix can be configured persistently with the command `indefinix configure (key) (value),(...)` or temporarily with optional flags in the short form `-(k) (value),(...)` or the long form `--(key) (value),(...)`.
Using the comma is optional in the command form, but mandatory in the flag forms while using spaces around the comma is optional in all forms.
Persistent configurations are saved to `~/.indefinix` if needed while temporary configurations are lost when the program exits.

The key `name`, abbreviated as `n`, contains the expected name of the index files.
The default value is `INDEX`.
Any file name is a valid `name`.

The key `editor`, abbreviated as `e`, contains the default text editor used for editing the index files with `indefinix edit` or the configuration file with `indefinix configure`.
The default value is empty unless a text editor is detected automatically.
Any path to an executable is a valid `editor`.

The key `order`, abbreviated as `o`, contains the order used in the index file.
The default value is `normal,directories,hidden`.
Any string in the form `(sorting),(grouping),(hiding)` is a valid order string, where `(sorting)` is either `normal`, `reverse` or `none`, `(grouping)` is either `directories`, `files` or `none` and `(hiding)` is either `hidden` or `none`.

The key `alignment`, abbreviated as `a`, contains the text alignment to use in the columns of the index files.
The default value is `left,left`.
Any single `(position)` or `(position),(position)` pair is valid `alignment`, where `(position)` is either `left`, `right` or `center`.
When given a single `(position)`, both of the columns are aligned together, and when given a `(position),(position)` pair, both of the columns are aligned independently.

The key `filling`, abbreviated as `f`, contains the text padding to use in the columns of the index files.
The default value is `fill,fill`.
Any `(padding),(padding)` pair is valid `filling`, where `(padding)` is either `fill` or `none`.

The key `infix`, abbreviated as `i`, contains the text that comes between the columns.
The default value is `"   "` (three spaces).

Similarly the keys `prefix`, abbreviated as `p`, and `suffix`, abbreviated as `s`, contain the texts that come before the first column and after the last column respectively.
The default values are both `""`.

Furthermore the keys `headinfix`, abbreviated as `hi`, `headprefix`, abbreviated as `hp`, and `headsuffix`, abbreviated as `hs`, contain the corresponding texts on the first line only.
The default values are empty except for `" - "` for `headinfix`.

Finally the keys `tailinfix`, abbreviated as `ti`, `tailprefix`, abbreviated as `tp`, and `tailsuffix`, abbreviated as `ts`, contain the corresponding texts on the last line only.
The default values are empty.

The key `what`, abbreviated as `w`, contains the texts that are displayed in exceptional situations.
The default value is `"not indexed","not present"`.

Any strings are valid texts.

### Usage

A brief usage reference can be printed by calling Indefinix without any arguments.

	[user@arch /tmp]$ indefinix

Indefinix can be used with the commands `indefinix (flags) (command) (argument) (...)`, where the `(flags)` are optional and the `(command)` can be partial if it's unambiguous.

The flags are explained in the previous section and the commands in the following subsections.

#### Configuration

The command `modify` opens the persistent configuration in the default text editor.

The command `configure (key) (value)` changes the persistent configuration by associating the given `(key)` with the given `(value)`.

The command `toss (key)` changes the persistent configuration by removing the given `(key)`.

The command `query (key)` looks up the given key in the persistent configuration.

The command `obliterate` deletes the persistent configuration.

#### Index

The command `edit` opens the index file in the default text editor.

The command `build` creates a new index file.

The command `grope` repairs the index file if it's mangled.

The command `add (entry) (description) (...)` adds the given `(entry)` to the index file with the given descriptions.

The command `remove (entry) (...)` removes the given entries from the index file.

The command `update (entry) (description) (...)` changes the given `(entry)` in the index file to have the given descriptions.

The command `find (string) (...)` searches the index file for the given strings.

The command `lookup (entry) (...)` looks up the given entries in the index file.

The command `destroy` deletes the index file.

#### Example

Let's first configure Indefinix into a Lisp fanatic style.

	[user@arch /tmp]$ indefinix configure alignment left
	[user@arch /tmp]$ indefinix configure filling none
	[user@arch /tmp]$ indefinix configure infix " ("
	[user@arch /tmp]$ indefinix configure prefix " "
	[user@arch /tmp]$ indefinix configure headprefix "("
	[user@arch /tmp]$ indefinix configure tailsuffix "))"

Let's make some dummy files and directories.

	[user@arch /tmp]$ touch README LICENSE
	[user@arch /tmp]$ mkdir documents music pictures

Let's create an index file and add some entries to it.

	[user@arch /tmp]$ indefinix build
	Created a new index.
	[user@arch /tmp]$ indefinix build
	An index already exists.
	[user@arch /tmp]$ indefinix add music sequential notes
	Added a new entry.
	[user@arch /tmp]$ indefinix add documents "books, papers and other text documents"
	Added a new entry.
	[user@arch /tmp]$ indefinix add LICENSE legal nonsense
	Added a new entry.

Let's take a look at the new index.

	[user@arch /tmp]$ indefinix lookup
	(music/ (sequential notes))
	(documents/ (books, papers and
	 other text documents))
	(LICENSE (legal nonsense))

Let's delete the configuration since it looks stupid.

	[user@arch /tmp]$ indefinix obliterate
	Do you really want to delete the configuration file "/home/user/.indefinix"? (y / N)
	y
	Destroyed the configuration.

Let's look up something in the index.

	[user@arch /tmp]$ indefinix lookup pictures music nothing
	pictures - not indexed
	music    - sequential notes
	nothing  - not present

Let's change the music description to something more descriptive.

	[user@arch /tmp]$ indefinix update music representations of pressure waves
	Updated an entry.

Let's find it.

	[user@arch /tmp]$ indefinix find wave
	music - representations of pressure waves

Let's remove some unnecessary things.

	[user@arch /tmp]$ indefinix remove documents
	Removed an entry.

Let's destroy the index.

	[user@arch /tmp]$ indefinix destroy
	Do you really want to delete the index file "/tmp/INDEX"? (y / N)
	y
	Destroyed the index.

It's a good idea to add Indefinix to the lookup `$PATH` and give it a shorter alias

	[user@arch /tmp]$ echo $PATH
	[user@arch /tmp]$ which indefinix
	[user@arch /tmp]$ alias ind=indefinix

to make life easier.

	[user@arch /tmp]$ ind l

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
