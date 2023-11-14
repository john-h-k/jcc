# Integers

Most C integer types have a minimum size, but not a strictly defined size.
For simplicity, JCC has a fixed size for integers across all architectures, as follow:

| Type        | Size (bytes) |
| `char`      | 1            |
| `short`     | 2            |
| `int`       | 4            |
| `long `     | 8            |
| `long long` | 8            |

These apply for both signed and unsigned (and in the case of `char`, the type without sign) types.

