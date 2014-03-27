calab-compiler
==============

The (o)Ca(mal) Lab(ouseur) Compiler to compile Alan's custom language to alan6502 Op Codes

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
