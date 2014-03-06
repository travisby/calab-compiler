calab-compiler
==============

The (o)Ca(mal) Lab(ouseur) Compiler to compile Alan's custom language to alan6502 Op Codes

Dependencies
------------
To install our dependencies, you need only:


$ opam install bolt

Bolt is our Logger utility.  You can see its configuration examples here: http://bolt.x9c.fr/manual.html


A sample log config is provided, that  will print all messages above (and including) the bolt level TRACE (the lowest level)


If it becomes too verbose, instead use WARN

Building
--------
After installing dependencies, you need only run our compile script


$ ./compile #  to build the ocaml project

Running
-------
The log config file is optional; if one is not provided, no logging will take place.


The default log provided will print to stdout


$ BOLT_FILE=log_config ./compiler filename
