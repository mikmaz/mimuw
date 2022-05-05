# CPyccino interpreter
###### Author:   Mikołaj Mazurczyk
###### Index no: 426819

The name of the language refers to:
- **C** - since it derives the most from *C* language
- **Py** - language contains *Python*-like tuples, and its `print()` function works similarly as in *Python*
- cappu**ccino** - language's grammar is based on Latte's

## Program

The program consists of top-definition functions, where the `main` function is required, as its body is evaluated during runtime.
Restrictions on the `main` function:
- it must return `int`
- it should take no arguments
- there can be only one `main` function declaration as a top definition

Every instruction of the program needs to end with a semicolon (except for blocks). CPyccino is a statically typed language, where every identifier has to be declared. Even though the code below `main` declaration isn't evaluated during runtime, it still undergoes type-checking.

## Types

Types supported by CPyccino:
- `int`, `boolean` and `string`
- `void`, which can only be used as function's return type
- `tuple< ... >`, which is a tuple type (more about tuples later in the file)

Additionally, every type `t` of the above (except for void) can be declared as read-only using `const t` (however, declarations with more than one `const` are considered errors, i.e. `const const int`).

If variable is declared without initialization, it is initialized with type's default value:
- `int` - `0`
- `boolean` - `false`
- `string` - `""`
- `tuple< [types] >` - tuple with default values of `[types]`

## print

Program can print values using `print([expressions])`. Instruction accepts all supported types (even `void`), every print ends with a newline character.

## while

Curly braces are obligatory.
```
while (true) {
    ...
}
```

## if

The language supports `if ...` and `if ... else ...` instructions. In both cases, curly braces are obligatory.

```
if (true) {
    ...
}
```

```
if (true) {
    ...
}
else {
    ...
}
```

## Functions

Functions can return every supported type. Also, language permits nested functions' declarations. Every function has to contain a `return` statement, except for `void` functions.

```
void fun([args]) {
    int nested_fun([args]) {
        ...
    }
    
    ...
}
```

## for

Instruction works similarly to  *Pascal's* for loop: iterator needs to be of type `const int`; loop's end value is only evaluated once during its runtime. Curly braces are obligatory.

```
for (const int i = 0 to 5) {
    ...
}
```

## tuples

Tuples are present in the language in three forms:
- as type
```
tuple <int, bool, tuple <string>> tpl;
```
- as expression, where every tuple needs to be surrounded by `<[` and `]>`
```
tuple <int, int> tpl = <[ 1, 2 ]>;
```
- as instruction of tuple pattern matching, where tuple's values on the right of the `=` operand are assigned to identifiers on the left position-wise
```
<x, <y, z>> = <[ 1, <[ 2, 3 ]> ]>; 
```

## Provided functionalities
| Nr | Funkcjonalność                                   | Zrealizowana |
|----|--------------------------------------------------|--------------|
| 1  | trzy typy                                        | +            |
| 2  | literały, arytmetyka, porównania                 | +            |
| 3  | zmienne, przypisanie                             | +            |
| 4  | print                                            | +            |
| 5  | while, if                                        | +            |
| 6  | funkcje, rekurencja                              | +            |
| 7  | zmienne read-only i pętla for                    | +            |
| 9  | przesłanianie i statyczne wiązanie               | +            |
| 10 | obsługa błędów wykonania                         | +            |
| 11 | funkcje zwracające wartość                       | +            |
| 12 | (4) statyczne typowanie                          | +            |
| 13 | (2) funkcje zagnieżdżone ze statycznym wiązaniem | +            |
| 14 | (1) tablice                                      |            - |
| 15 | (2) krotki z przypisaniem                        | +            |
| 16 | (1) break, continue                              |            - |

**Razem: 28 pkt**
