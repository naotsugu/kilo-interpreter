# Kilo Interpreter

Kilo Interpreter is a small interpreter in less than 1K lines of code (counted with cloc).

```
$ brew install cloc
$ cloc src/main/java
----------------------------------------------------------------------
Language         files          blank        comment           code
----------------------------------------------------------------------
Java                 2            190            179            986
----------------------------------------------------------------------
SUM:                 2            190            179            986
----------------------------------------------------------------------

```

Kilo Interpreter does not depend on any library.

Original is [hear](https://interpreterbook.com/).


## Usage

Kilo Interpreter is Single-File Source-Code Programs.

So just run the following command:

```
$ java src/main/java/com/mammb/kilo/Interpreter.java
```


## Example

Create source file.
 
factorial.mk
```c
let factorial = fn(n) {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
};
factorial(5);
```

Run it :
```
$ java src/main/java/com/mammb/kilo/Interpreter.java factorial.mk
120
```


