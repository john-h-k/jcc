
# Known issues

Scalar array initializers do not work as expected.

```c
struct foo {
  int a[2];
};

struct foo f = { 1, 2, };
```

is legal, but JCC currently rejects it;

On systems where `ftell` fails for `stderr`, certain debug functions that attempt to pad their output will fail and enter a loop (known: `aarch64_debug_print_codegen`)
