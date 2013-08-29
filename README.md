# Indefinix

Indefinix is a command line directory indexer.
Its purpose is to maintain a plain table of contents found in a directory.
The name is a distasteful mix of the words "index" and "definition".

## Schedule

This project isn't finished yet.

## Motivation

Finding out what certain files or directories are for requires looking inside them, reading about them somewhere or asking someone.
Sometimes even that's not sufficient.
Using an index solves that problem.


## Installation

Indefinix is written in C and works on any system.

The following examples are from a (freshly installed) Arch Linux.

### Binaries

Binaries only need to be downloaded and extracted.

	[user@arch ~]$ wget https://github.com/Tuplanolla/indefinix/blob/master/indefinix.tar.gz
	[user@arch ~]$ tar -xf indefinix.tar.gz

They may be older than the sources.

### Sources

Building Indefinix relies on the GNU Compiler Collection and GNU Make although
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

The binaries go in the `bin` directory and
 temporary objects in the `obj` directory.
The object files can be removed after compilation.

	[user@arch indefinix]$ rm -f obj/*

## Running

Running the launcher for the first time will
 not generate a template configuration file in the current working directory.

	[user@arch indefinix]$ bin/indefinix

The configuration file may need to be edited.
The process of doing so is addressed in its own section.

	[user@arch indefinix]$ nano indefinix.cfg

After taking care of the configuration file the launcher will start properly.

	[user@arch indefinix]$ bin/indefinix

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
