# A* in Brainfuck

This is an advanced example of what can be achieved with Brainzen. [a-star.bz](a-star.bz) contains a Brainzen program that implements the [A* algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm) for a 2-dimensional grid of fixed size (which can be changed by modifying the constants `#WIDTH` and `#HEIGHT`).

## How to use

The compiled Brainfuck program (which you can compile yourself from the source using the [Brainzen compiler](https://github.com/MDLC01/brainzen), or [download from this repo as a zipped Brainfuck source file](a-star.zip) for a version which expects a 9×9 grid) reads a grid from standard input and outputs a path to standard output.

In case no path exist for the specified grid, the behavior of the program is undefined, but likely an infinite loop.

### Grid format

A grid of height _m_ and width _n_ is specified by _m_ lines of _n_ characters.

Lines are separated by either:

* A carriage return, followed by any character; or
* Any character but a carriage return.

Within a line, each character describes a cell, with the following rules:

* A `#` defines a wall (a path cannot go through a wall);
* A `S` marks the cell as the starting cell;
* A `G` marks the cell as the goal;
* Any other character marks the cell as a normal cell.

See [grid1.txt](grid1.txt) and [grid2.txt](grid2.txt) for examples of grids.

### What is a shortest path?

A path is the data of a way to go from a start cell to a target cell using only horizontal and vertical movements of one cell at a time, without using wall cells.

A shortest path from a cell A to a cell B is a path from A to B such that no path from A to B exists with a shorter length (meaning, using a strictly smaller amount of cells).

### Path format

This program outputs the path that it found in a human-readable form. Wall cells are indicated by `##` and the source cell by `()`. Every cell that does not contain `??` contains the name of its parent, meaning the name of the previous cell in the path. To reconstruct the path, start at the goal cell and work your way up the parents from cell to cell until you reach the source cell.
