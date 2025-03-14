# The Register Allocation Problem

Computers have a finite number of registers, but SSA has an arbitrary number of values.

> Recap of SSA:
>   * Every value is defined exactly once, and used zero or more times
>   * When a decision between two variables must be made, a `phi` node is used

Each operand in SSA needs either no register, or a certain type of register. In reality, these are "integer" or "floating-point", but you can consider them as being arbitrary classes.
Each class has a certain number of available registers, split into two groups:

* Volatile - a _volatile_ register is one where it must be saved and reloaded across a `call` boundary, as the callee is free to use that register
* Non-volatile - a _non-volatile_ register is one where we must save its prior value at the start of the function, and restore at the end, but can use freely without being concerned that it will be overwritten by `call`s

The save/restore of non-volatile registers can be ignored and does not affect register allocation.

Control flow in general is represented as a list of _basicblocks_. A basicblock has the property that if any instruction within the block is executed, they are all executed (it has no control-flow).
There is a single entry basicblock, and then each block may have 0 or more successors. A basicblock with 0 successors is either dead, or returns a value. Dead basicblocks are removed before regalloc and can be ignored.

Control flow is not necessarily reducible. However, it is guaranteed that you can easily iterate basicblocks in reverse-post-order.

The goal by the end of register allocation is that every operation has a register. Of course, this may not be possible initially due to having too many live variables.
You can fix this by performing a _spill_, where you write the register value to a stack, and then reload it before a usage.

```
%0 = 10

// other instructions, which use all the registers so none are available for %0

%10 = %0 + 5
```

would be transformed to

```
%0 = 10
store.lcl [LCL(0)], %0

// other instructions, which can use registers freely as %0 does not occupy one

%10 = load.lcl [LCL(0)]
%11 = %10 + 5
```

This reduces the "live range" of `%0` from being `[%0, %10]` to just `[%0, %1]`, and then introduces a new live range for its use.
It is important that these new tiny ranges are never spilled, else we cannot guarantee register allocation terminates, because you could end up in a scenario where each time a reload is introduced, it is then spilled again, ad infinitum.

A big caveat of spilling is that spilling and reloading itself can require new registers.
Consider reloading a value to stack position `5000` on hardware that only supports immediate offsets of `4096`. You must instead do
```
  mov some_reg, stack_pointer
  add some_reg, 5000
  load dest_reg, [some_reg]
```

If `dest_reg` is a class of register that can be used for addressing, you can reuse it. But this is not guaranteed, for example, on most architectures, floating-point registers
cannot be used for addressing. So if this was a reload of a floating-point value, we would need another addressing-class register.

The same logic applies for spilling, just `store` rather than `load`. In the majority of cases, you will not need an extra reg, because it will be a low enough offset, but it must be supported.

## Register requirements

Certain operations require values to be in specific registers. There are effectively two classes:

* Fixed-operand instructions
  - Example: `DIV` on `x86_64`.
    - This performs a division of the 128-bit value in registers `RDX` (high bits) and `RAX` (low bits), placing the quotient in `RAX` and remainder in `RDX`
    - We don't care about the high bits, but this means it effectively requires `RDX` to be zero (you can also treat this as it "writing to `RAX`")
    - It also puts the output in a specific register, `RAX`
* Function calls:
  - Arguments for calls must go in specific registers
  - The return value is also in a specific register

Determining these registers is not the job of the register allocator, and they have all been inserted into the code. However, it must respect these.
Additionally, it is beneficial for it to consider these in other assignments. For example

```
%0 = 10
%1 = %0 register=R0 // this is a mov instruction to put the value into the correct register, %0 has no register here
call foo ( %1 )
```

In this case, we want the register allocator to try and put `%0` in register `R0` so it doesn't introduce a meaningless mov afterwards. The move will still exist in IR, but if it is between the same reg, it can be eliminated after regalloc.
An immediate question is "why does the move exist?". This is because it reduces the live-range of the _fixed_ value to the smallest possible one.

Consider

```
%0 = 10 register=R0
%1 = 20 register=$0
call foo ( %0 )
call bar ( %1 )
```

This will fail, because there are overlapping ranges with fixed registers. So instead, all fixed-register operands get split:
* If their output is fixed, a move directly afterwards will be inserted and all consumers will use that
* If their input is fixed, a move will be inserted directly before use that fixes the register


