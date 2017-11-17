OCaml VM
------------------------

This is a OCaml virtual machine that is able to compile and run
basic OCaml code. 

Syntax
------------------------

The only type implemented into the VM for the moment are integers,
References and functions are also implemented (one variable functions).
An addition to OCaml's original language is that this is a Concurrent
Virtual Machine, so there is a command that allows you to spawn new threads
for applying function operators. Its syntax is the following:
	
{expr} : Expressions

{atom} : Atomic Expressions

{op} : Operators

  {expr} ::= {atom}                 (Atomic)

   \|  {expr} {op} {expr}           (Binary Operation)
   
   \|  let [id] = {expr} in {expr}  (Local Definition)

   |
   
   \| [id] -> {expr}           (Function)
   
   \| {expr} {atom}                (Application)

   |

   \|  ref {atom}                   (Reference)      
   
   \|  {expr} := {expr}             (Update)
   
   \|  {expr} ; {expr}              (Sequence)

   |

   \|  spawn {atom} {atom}          (New thread)


  {atom} ::= [n]                    (Constant integer)
   \|  [id]                         (Name)
   
   \|  ( {expr} )                   (Expression)

   |
    
   \|  ! {atom}                     (Reads)

  {op} ::= + | - | *        (Operator)

Quick Start
------------------------

In order to compile and run your OCaml code, just do make within the
directory then run ./VM.o ~your ocaml script here~ from within your
terminal. There are sample scripts in the Tests directory.