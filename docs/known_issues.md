
# Known issues

Scalar array initializers do not work as expected.

```c
struct foo {
  int a[2];
};

struct foo f = { 1, 2, };
```

is legal, but JCC currently rejects it;
