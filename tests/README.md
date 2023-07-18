# Tests

This directory contains tests that can be executed using `cargo test`.

## Structure

The `tests.bz` file contains a `Tests` module. Each procedure in this, module is a test. Tests must be valid Brainzen. The resulting Brainfuck code is interpreted, and its output is compared against a reference stored in [`reference/`](reference).

## How to write a test

To write a test, create a new procedure under the `Tests` module. A test should not accept any argument, and its return value is ignored. Any read operation (e.g., call to `input`) is illegal.

Using the `@<expression>` syntax, you can assert that an expression evaluates to 1. For example:
```brainzen
@(1 < 2);
```
You can assert an expression does not evaluate to 1 using `@!`:
```brainzen
@!(1 > 2);
```
For a test to pass, all `@` assertions must pass.

Anything a test writes (e.g., using `print`) is compared for byte by byte equality to the reference stored in the file with the corresponding name under [`reference/`](reference). If no such file exist, it will be created with the output of the test as its content. This lets you update references easily by simply deleting them. Be careful when opening a reference file in your editor, as they may contain non-printable characters.
