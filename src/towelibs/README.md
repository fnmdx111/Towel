Extending Towel as a General-Purposed Language
====

I have recently added an interface for extending the Towel programming language with custom libraries.

Prerequisites
----

The library (no matter if it's third-party, or of your own) should be **OCaml-compliant**, because TVM is built on top of OCaml too. That is to say, you should be able to write OCaml programs with it, thus TVM will have access to it via dynamic linking.

Approach
----

Two instructions were implemented in TVM: `load-ext` and `extcall`.

`load-ext` takes an `ext_str` from the stack and loads the corresponding extension module into TVM via `Dynlink`. An `ext_str` is a filename of the OCaml object file (always with the `.cmo` extension even if it's a `.cmxs` file). After successfully loading it, the instruction leaves an unsigned integer (`OVUFixedInt`) on the stack for future referencing the extension.

`extcall` takes two arguments from the stack: it first takes the TOS as the extension handle (the one `load-ext` pushed), then it takes another unsigned integer as the extension function call number (although this integer will be converted into an OCaml `int`, but I think 2 ** 31 is fairly enough for ordinary libraries). The extension module then matchs against the call number to determine which action to perform on the data stack.

Howto
----

(The following step illustrates how to build native extension rather than bytecode extension. If you want bytecode extension, use `ocamlc` instead, and probably lose some of the parameters.)

The first step, create a OCaml plugin (don't know why `.cmxs` are called plugins). This plugin file is essentially a dynamic library (e.g. a `.so` file on *nix, a `.dll` file on Windows). When implementing this plugin, you must implement the module interface `TowelExtTemplate` laid out in `src/vm/ext.ml`. It's simple: one function - `extcall`. You will have to route individual call numbers to different routines using this function.

The first argument of `extcall` is the call number, then the second one is the `dss` of the VM, for you to get specific arguments from the data stack or leave your result on it. But be careful, if you ruin this dss, TVM is doomed.

After you finish your extcall function and other local functions, remember to register your module value in the `__ext__` slot so that when loading this extension, TVM will get to know of your extension.

The second step, you compile your source code via the following command:

        ocamlfind ocamlopt -package %1 %2 -shared -linkall -I %3 -o %4

Of these %parameters:

        (%1) the packages your extension module uses, take `ext_random.ml` as an example, %1 will be replaced by stdint;
        (%2) the extension module source file, `ext_random.ml`, for example;
        (%3) the directory where `t.cmi` and `nstack.cmi` are;
        (%4) the output binary filename, `ext_random.cmxs` for exmpale.

The command for compiling the Random extension module is like this:

        ocamlfind ocamlopt -package stdint ext_random.ml -shared -linkall -I ../../build/src/vm -o ext_random.cmxs

Third, place the `.cmxs` file in the `libpaths` of TVM, default locations are `cwd` and where `towelib` is at. Modify `src/vm/config.ml` to change these locations.

Last, write a Towel wrapper module for your extension module. In the `.w` instruction wrapper module, `!>ext` is for the `load-ext` instruction and `!>>` is for `extcall`. You should always bind a name to the extension handle pushed by `!>ext`. It is a good idea to bind a local partial function with the handle already applied. Not exporting both the handle and this function are highly recommended.

Roadmap
----

All other complicated operations scheduled in Towelib (such as file operations, etc.) will be implemented in this way. I have already implemented the Random module using this approach.

Example
----

See `ext_random.ml` and `random.t`.

