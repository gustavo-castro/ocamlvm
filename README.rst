OCaml VM
------------------------

This is a OCaml virtual machine that is able to compile and run
basic OCaml code. 

Syntax
------------------------

The only type implemented into the VM for the moment are integers,
References and functions are also implemented (one variable functions).
OCaml's Let's can only be used coupled with the in keyword. An addition
to OCaml's original language is that the this is a Concurrent Virtual Machine,
so there is a command that allows you to spawn new threads for applying
function operators, and it's syntax is
spawn ~your function goes here~ ~your variable goes here~.

Quick Start
------------------------

In order to compile and run your OCaml code, just do make within the
directory then run ./VM.o ~your ocaml script here~ from within your
terminal. There are sample scripts in the Tests directory.