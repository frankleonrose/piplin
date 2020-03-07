# Plan of Work

## Code Cleanup
- Look at ~20 TODO items
- Move mips files into micros namespace
- Move Z80 into micros namespace
- Enable defining a module one and calling it in  multiple places to instantiate multiple times.
- Compiler error if input overwrites state key.
- Compiler warning if input does not match a key used by graph.
- Compiler error if module caller extracts a key that is not populated by module. (fnk [x] (:output-y (module :input x))) How would that be done?

## Update to ICE40 capability
- Support "parameter" in Verilog output? Or is it unnecessary because we support constants in Piplin code? Yeah, seems unnecessary, except to define primitive attributes.
- Look into .pcf files. Any need to represent that or generate it from Piplin?
- Support defining device primitives.
- Support external modules - include a file, expect the module to be defined, express inputs and outputs to it, and associate a function that will simulate it.

## Defining a Device Primitive
1. (def SB_IO (device-primitive "SB_IO" {parameter names and types and defaults} {wire names and types} simulation-function))
2. (let [io1 (SB_IO configuring-parameters)]) - Like (modulize) returns a compiling function
3. (fnk [] (:output (io1 :input 123))) - Calls compiling function to populate `*state-elements*`
Device primitives insert an AST nodes in the hierarchical name space of `*state-elements*`, like [mod1...mod2 dp-name].
This AST node is used by `(->verilog)` function to know what to instantiate. In addition, all outputs of the device primitive
are published in `*state-elements*` as well. For instance, :pin-state output would appear as a wire whose value was determined
by the primitive, like a register, but without needing to declare a register.

## Questions
- Is `*state-elemements*` updated only for root compiled module? No. (fnk) are not evaluated until compile time.
- If `*state-elemements*` need to be collected for all modules, how does this work? What about calling module function twice? Error case would be to call module with two separate arguments and see two outputs.
- How to get multiple outputs of a module? (let [{:keys a b c} (module-fn :in1 in1 :in2 in2)] (stuff-with a b c))
