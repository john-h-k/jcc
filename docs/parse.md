# Parse

The parsing stage involves 4 principal stages:

* Preprocessing
  - Runs the C preprocessor, handling `#` directives
* Lexing
  - Tokenises the output of the preprocessor
* Parsing
  - Builds a highly generic AST which can parse a much larger set of programs than are legal in C
  - This allows emitting significantly clearer diagnostics than if it parsed more strictly
* Typing
  - Types the tree and performs most validation to reject invalid programs

