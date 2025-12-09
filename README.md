# Ophy

Ophy is a physics library written in OCaml, providing modules for geometric primitives (Point, Circle, Rectangle, Line) and collision detection functionalities. The collision detection module `Collide` takes inspiration from Jeffrey Thompson's work on Collision Detection.

## Features

-   **Point**: Represents a 2D point with utility functions like distance calculation.
-   **Circle**: Represents a circle with functions for area, diameter, and circumference.
-   **Rect**: Represents a rectangle with functions for area, perimeter, and diagonal.
-   **Line**: Represents a line segment with a function for calculating slope.
-   **Collide**: A comprehensive module for checking collisions between various geometric shapes (Point-to-Point, Point-to-Circle, Circle-to-Circle, Point-to-Rectangle, Rectangle-to-Rectangle, Circle-to-Rectangle, Line-to-Point, Line-to-Circle, Line-to-Line).

## Installation

To install Ophy, you will need `opam` (the OCaml package manager) and `dune` (the build system).

1.  **Install OCaml and OPAM**:
    If you don't have OCaml and OPAM installed, follow the instructions on the [OCaml website](https://ocaml.org/docs/install.html).

2.  **Clone the repository**:
    ```bash
    git clone https://github.com/your-username/ophy.git
    cd ophy
    ```

3.  **Install dependencies**:
    ```bash
    opam install . --deps-only
    ```

## Building

To build the project:

```bash
dune build
```

This will compile the `ophy` library.

## Running Tests

Ophy uses `dune` for running tests, including inline tests.

To run all tests:

```bash
dune runtest
```
