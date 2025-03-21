# Typechk

The typechk phase performs semantic analyis and validation, turning the generic parse AST into a typed AST of legal C code. It keeps the namespacing convention the rest of the project uses by having a short prefix for all public types, in this case `td` (for **t**ype**d**)

It is mostly a mapping over the parser AST, but not all types are 1:1. For example, a declaration in the AST is:

```c
struct ast_declaration {
  struct ast_declaration_specifier_list specifier_list;
  struct ast_init_declarator_list declarator_list;

  struct text_span span;
};
```

However, post typing, it looks like this:

```c
struct td_declaration {
  enum td_storage_class_specifier storage_class_specifier;
  enum td_function_specifier_flags function_specifier_flags;

  struct td_var_ty base_ty;

  size_t num_var_declarations;
  struct td_var_declaration *var_declarations;
};
```

The declaration & type specifiers have been split into a more canonical format which makes analysis and building the IR more simple. Storage and function specifiers are explicitly stored, and the declaration type stored in `base_ty`, with each `td_var_declaration` containing the type of the declaration.

This phase is also responsible for folding all expressions which are constant at compile time. For example, an array size is a expression within the parse AST, but here it is finalised into the numeric value of the array size, or rejected with a diagnostic if it is not a valid constant expression.

The majority of diagnostics, such as variables not existing, invalid casts, calling things that are not functions, etc, are emitted here. It also performs some canonicalisation of the AST so that the IR builder does not need to concern itself with intricacies of the C spec. In C, both `arr[10]` and `10[arr]` are legal, but typechk will ensure that the left hand side of an array-access expression is the pointer type and the right hand side is the offset (this is legal as there is no sequencing point within an array-access expression).

### Debugging

Similarly to the parse stage, typechk has the the ability to prettyprint its AST.

You can compare the TD AST to the parse AST from [Parse](./parse.html). Note that all expressions and variables are typed

```text
PRINTING TD
DECLARATION
STORAGE CLASS SPECIFIER
    NONE
VARS
    VAR DECLARATION
        VARIABLE 'printf'
        SCOPE GLOBAL
    TYPE
    VARIADIC FUNC (
        POINTER TO
            signed char
    ) RETURNS
        int
FUNCTION DEFINITION
STORAGE CLASS
    NONE
VAR DECLARATION
    VARIABLE 'main'
    SCOPE GLOBAL
TYPE
UNSPECIFIED FUNC (
) RETURNS
    int
BODY
    COMPOUND STATEMENT:
            EXPRESSION
                int
                COMPOUND EXPRESSION:
                    EXPRESSION
                        int
                        CALL
                            TARGET
                            EXPRESSION
                                VARIADIC FUNC (
                                    POINTER TO
                                        signed char
                                ) RETURNS
                                    int
                                VARIABLE 'printf'
                                SCOPE GLOBAL
                            ARGLIST:
                                EXPRESSION
                                    CONST
                                    POINTER TO
                                        char
                                    CONSTANT "Hello, World!\n"
            RETURN
                EXPRESSION
                    int
                    CONSTANT 0
```
