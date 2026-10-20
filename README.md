# symengine.f90

Fortran wrappers of SymEngine

## Installation

Install SymEngine:

    cmake -DCMAKE_INSTALL_PREFIX=/some/path .
    make
    make install

Install SymEngine.f90:

    cmake -DCMAKE_PREFIX_PATH=/some/path .
    make
    make install

### Using fpm

fpm does not know how SymEngine was built, so the libraries to link must be
given with `--link-flag`: `-lsymengine`, followed, for a static SymEngine (the
default), by the libraries it was built with (e.g. `-lgmp`, `-lLLVM`, `-lflint`,
`-lmpfr`) and the C++ runtime. For example, for a SymEngine installed in
`/some/path` and built with GMP and LLVM:

On Linux (fpm passes `--link-flag` before the object files, so the GNU linker
needs `--whole-archive` to keep SymEngine):

    fpm test --flag "-DHAVE_SYMENGINE_LLVM" --link-flag "-L/some/path/lib \
        -Wl,--whole-archive -lsymengine -Wl,--no-whole-archive \
        -L$(llvm-config --libdir) -lgmp -lLLVM -lstdc++"

On macOS, with GMP and LLVM from Homebrew:

    fpm test --flag "-DHAVE_SYMENGINE_LLVM" --link-flag "-L/some/path/lib \
        -L$(brew --prefix)/lib -L$(brew --prefix llvm)/lib -lsymengine -lgmp -lLLVM -lc++"

`-DHAVE_SYMENGINE_LLVM` enables the LLVM backend of `Lambdify` and should only
be passed if SymEngine was built with `WITH_LLVM=ON`.

## Lambdify

`Lambdify` turns expressions into numerical functions, like `Lambdify` in
symengine.py:

```fortran
use symengine
type(Basic) :: x, y
type(Lambdify) :: f
real(c_double) :: out(1), inps(2, 100), outs(1, 100)
procedure(lambdify_function), pointer :: fn

x = Symbol('x')
y = Symbol('y')
call f%init([ptr(x), ptr(y)], [ptr(x**2 + y**2)])   ! or arrays of type(Basic)
call f%eval([1.0d0, 2.0d0], out)                   ! one point
call f%eval(inps, outs)                            ! inps(2, n) -> outs(1, n)
```

Optional arguments of `init`: `cse` (common subexpression elimination),
`backend` (`'lambda'`, the default, or `'llvm'`) and `opt_level` (LLVM only).
With `backend='llvm'`, `call f%get_function(fn)` returns the compiled function
as a procedure pointer, called as `call fn(inps, outs)`. The LLVM backend
requires SymEngine built with `WITH_LLVM=ON` (with fpm, see above). A
`Lambdify` object cannot be copied, and with the `'lambda'` backend each thread
needs its own object.
