# Kilo Interpreter

Kilo Interpreter is a small interpreter in less than 1K lines of code (counted with cloc).

```
$ brew install cloc
$ cloc src/main/java
----------------------------------------------------------------------
Language         files          blank        comment           code
----------------------------------------------------------------------
Java                 2            176            157            913
----------------------------------------------------------------------
SUM:                 2            176            157            913
----------------------------------------------------------------------
```

Kilo Interpreter does not depend on any library.

Original is (hear)[https://interpreterbook.com/].


## Usage

JDK 14+ is required(with `--enable-preview`).

Kilo Interpreter is Single-File Source-Code Programs.

So just run the following command:

```
$ java --source 14 --enable-preview src/main/java/com/mammb/kilo/Interpreter.java
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
$ java --source 14 --enable-preview \
src/main/java/com/mammb/kilo/Interpreter.java \
factorial.mk
120
```

