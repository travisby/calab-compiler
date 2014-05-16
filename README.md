calab-compiler
==============

The (o)Ca(mal) Lab(ouseur) Compiler to compile Alan's custom language to alan6502 Op Codes

Known Bugs
----------
You cannot have a loop as the first action of a program.  
We use Mem[0x00] for temporary variables, and a while()
at the top of the file will need to reuse Mem[0x00]

Variables with the same name.  Please, just don't try to have two a's.  You'll only be hurting yourself (and my grade)

Dependencies
------------

Install OCaml

Logging
-------

Grep for what you are looking for (or not).  We use TRACE/INFO/WARN/ERROR/FATAL

Building
--------
After installing dependencies, you need only run our compile script


$ ./compile #  to build the ocaml project

Running
-------

$ ./compiler filename

Tests
-----
To run our test suite, simply build the application,
and from the project root directory:


$ ./test


It will print some fun output on the errors and warnings for each test case
