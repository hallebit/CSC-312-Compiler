#   Project: CSC-312-Compiler 
A compiler written in OCaml for Grinnell College's CSC 312 course with [Peter-Michael Osera](https://github.com/psosera).

### Contributor(s): 
Halle Remash | halle@remash.com

### Setup 
- Download this repository into your directory
- Navigate to the `src` directory in this project in your terminal
- Build the file using the `make` command 

### Dependencies
- Install [OCaml](https://ocaml.org/docs/install.html)
- Ensure that you have installed [merlin](https://opam.ocaml.org/packages/merlin/)
  - If you used the [OPAM](https://opam.ocaml.org/) package manager, type `opam search merlin`
  - Else see the [merlin's README](https://github.com/ocaml/merlin) to install
- Ensure that you have installed [menhir](http://gallium.inria.fr/~fpottier/menhir/)
  - If you used the OPAM package manager, type `opam install menhir`
- Install [utop](https://opam.ocaml.org/packages/utop/) [(about)](https://opam.ocaml.org/blog/about-utop/)
  - If you used the OPAM package manager, type `opam install utop`
      
### Makefile Commands
The Makefile for this project is located in the `./src` directory. Navigate there in order to run Makefile commands.
- `make`        : build project using ocamlbuild 
- `make clean`  : runs ocamlbuild's clean
- `make cleanup`: removes automatically generated files
- `make test`   : runs test.sh, the project test suite
- `make top`    : runs utop (type `#quit;;` to exit)

### Excecution Instructions
`make` currently produces the file `compiler.byte` which can be run in the terminal.
Note that this project compiles byte-code instead of native code.
Also note that `./compiler.byte` can be excecuted with `-help` and `-parse` flags in 
addition to the `.src` file that it "compiles" 

### Current Language Syntax 
```
e ::= n | (e) | e1 + e1 | e1 - e2 | e1 * e2 | e1 / e2
        | true | false | e1 <= e2 | if e1 then e2 else e3
```
