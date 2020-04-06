# Plan of Work

## Code Cleanup
- Look at ~20 TODO items
- Move mips files into micros namespace
- Move Z80 into micros namespace
X- Enable defining a module once and calling it in multiple places to instantiate multiple times. Made name a gensym. There may be more to this.
- Compiler error if input overwrites state key.
- Compiler warning if input does not match a key used by graph.
- Compiler error if module caller extracts a key that is not populated by module. (fnk [x] (:output-y (module :input x))) How would that be done?

## Update to ICE40 capability
X(moot)- Support "parameter" in Verilog output? Or is it unnecessary because we support constants in Piplin code? Yeah, seems unnecessary, except to define primitive attributes.
- Look into .pcf files. Any need to represent that or generate it from Piplin?
- Support defining device primitives.
- Support external modules - include a file, expect the module to be defined, express inputs and outputs to it, and associate a function that will simulate it.

### TODO Primitives
X- Generate output values as Verilog wires that are connected to output of primitive and accessible via bundle key
- Support adding specific types to primitive ports
- Support `:inout` primitive port
- Support simulating modules with primitives - specify a simulation function
### TODO Replicate blink.v from Fomu docs
- Wire output of device primitive
- Don't expose ALL state variables as outputs
- Explicit named module inputs (matching pre-defined pin names, for instance)
- Explicit named module outputs (again, matching pin names)


## Verilog Features
- Notes: implicit naming "input X (X)" can be written "input X"
- Unconnnected ports: "unconnected ( )"
- net type: wire, tri, wand, supply0
- variable type: reg, integer, time, real, realtime
- SystemVerilog Data Objects: wire, reg, wand (4 state: 0, 1, Z, X) and bit, byte, shortint, int, longint (2 state: 0, 1)
- SystemVerilog: "logic" is the 4-state type


## Defining a Device Primitive
1. `(def SB_IO (device-primitive "SB_IO" {parameter names and types and defaults} {wire names and types} simulation-function))`
2. `(let [io1 (SB_IO configuring-parameters)])` - Like (modulize) returns a compiling function
3. `(fnk [] (:output (io1 {:input 123})))` - Calls compiling function to populate `*state-elements*`
Device primitives are represented by an AST `{:fn {:op :primitive-instance}}` node in the compiled module.
The return value is a Bundle type with fields for each of the outputs of the primitive node.
That output may be assigned to a variable in the module's computation map and each output may be accessed
using the `:bundle-key` operation, which looks like keyword lookup: `(:output-name primitive-ouput)`.
This AST node is used by `(->verilog)` function to know what to instantiate. In addition, all outputs of the device primitive
are published in `*state-elements*` as well. For instance, :pin-state output would appear as a wire whose value was determined
by the primitive, like a register, but without needing to declare a register.

## Questions
- Is `*state-elemements*` updated only for root compiled module? No. (fnk) are not evaluated until compile time.
- If `*state-elemements*` need to be collected for all modules, how does this work? What about calling module function twice? Error case would be to call module with two separate arguments and see two outputs.
- How to get multiple outputs of a module? (let [{:keys a b c} (module-fn :in1 in1 :in2 in2)] (stuff-with a b c))
