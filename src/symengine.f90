module symengine

use exceptions
use iso_c_binding, only: c_size_t, c_int, c_long, c_double, c_char, c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_associated
use iso_fortran_env, only: int32, int64, real32, real64
implicit none

interface
    function c_strlen(string) bind(C, name="strlen")
        import :: c_size_t, c_ptr
        type(c_ptr), intent(in), value :: string
        integer(kind=c_size_t) :: c_strlen
    end function c_strlen
    function c_basic_new_heap() bind(c, name='basic_new_heap')
        import :: c_ptr
        type(c_ptr) :: c_basic_new_heap
    end function
    subroutine c_basic_free_heap(s) bind(c, name='basic_free_heap')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_basic_assign(a, b) bind(c, name='basic_assign')
        import c_long, c_ptr
        type(c_ptr), value :: a, b
        integer(c_long) :: c_basic_assign
    end function
    function c_basic_str(s) bind(c, name='basic_str')
        import :: c_ptr
        type(c_ptr), value :: s
        type(c_ptr) :: c_basic_str
    end function
    function c_basic_parse(s, c) bind(c, name='basic_parse')
        import c_long, c_ptr, c_char
        type(c_ptr), value :: s
        character(kind=c_char), dimension(*) :: c
        integer(c_long) :: c_basic_parse
    end function
    subroutine c_basic_str_free(s) bind(c, name='basic_str_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_basic_add(s, a, b) bind(c, name='basic_add')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: c_basic_add
    end function
    function c_basic_sub(s, a, b) bind(c, name='basic_sub')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: c_basic_sub
    end function
    function c_basic_mul(s, a, b) bind(c, name='basic_mul')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: c_basic_mul
    end function
    function c_basic_div(s, a, b) bind(c, name='basic_div')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: c_basic_div
    end function
    function c_basic_pow(s, a, b) bind(c, name='basic_pow')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: c_basic_pow
    end function
    function c_basic_eq(a, b) bind(c, name='basic_eq')
        import :: c_int, c_ptr
        type(c_ptr), value :: a, b
        integer(c_int) :: c_basic_eq
    end function
    function c_basic_neq(a, b) bind(c, name='basic_neq')
        import :: c_int, c_ptr
        type(c_ptr), value :: a, b
        integer(c_int) :: c_basic_neq
    end function
    function c_basic_sin(s, a) bind(c, name='basic_sin')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: c_basic_sin
    end function
    function c_basic_cos(s, a) bind(c, name='basic_cos')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: c_basic_cos
    end function
    function c_basic_exp(s, a) result(res) bind(c, name='basic_exp')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_basic_log(s, a) result(res) bind(c, name='basic_log')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_basic_abs(s, a) result(res) bind(c, name='basic_abs')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_basic_sqrt(s, a) bind(c, name='basic_sqrt')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: c_basic_sqrt
    end function
    function c_basic_atan2(s, a, b) result(res) bind(c, name='basic_atan2')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_basic_evalf(s, b, bits, domain) bind(c, name='basic_evalf')
        import :: c_long, c_int, c_ptr
        type(c_ptr), value :: s
        type(c_ptr), value :: b
        integer(c_long), value :: bits
        integer(c_int), value :: domain
        integer(c_long) :: c_basic_evalf
    end function
    subroutine c_basic_const_pi(s) bind(c, name='basic_const_pi')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_e(s) bind(c, name='basic_const_E')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_euler_gamma(s) bind(c, name='basic_const_EulerGamma')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_catalan(s) bind(c, name='basic_const_Catalan')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_goldenratio(s) bind(c, name='basic_const_GoldenRatio')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_basic_max(s, d) bind(c, name='basic_max')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, d
        integer(c_long) :: c_basic_max
    end function
    function c_basic_subs2(s, e, a, b) bind(c, name='basic_subs2')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, e, a, b
        integer(c_long) :: c_basic_subs2
    end function
    function c_basic_free_symbols(self, symbols) result(res) bind(c, name='basic_free_symbols')
        import :: c_long, c_ptr
        type(c_ptr), value :: self, symbols
        integer(c_long) :: res
    end function
    function c_integer_set_si(s, i) bind(c, name='integer_set_si')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: i
        integer(c_long) :: c_integer_set_si
    end function
    function c_integer_get_si(s) bind(c, name='integer_get_si')
        import c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long) :: c_integer_get_si
    end function
    function c_rational_set_si(s, a, b) bind(c, name='rational_set_si')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: a, b
        integer(c_long) :: c_rational_set_si
    end function
    function c_real_double_set_d(s, d) bind(c, name='real_double_set_d')
        import :: c_double, c_long, c_ptr
        type(c_ptr), value :: s
        real(c_double), value :: d
        integer(c_long) :: c_real_double_set_d
    end function
    function c_complex_set(s, re, im) result(res) bind(c, name='complex_set')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, re, im
        integer(c_long) :: res
    end function
    function c_symbol_set(s, c) bind(c, name='symbol_set')
        import c_long, c_ptr, c_char
        type(c_ptr), value :: s
        character(kind=c_char), dimension(*) :: c
        integer(c_long) :: c_symbol_set
    end function
    function c_setbasic_new() result(res) bind(c, name='setbasic_new')
        import :: c_ptr
        type(c_ptr) :: res
    end function
    subroutine c_setbasic_free(s) bind(c, name='setbasic_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_setbasic_get(s, n, r) bind(c, name='setbasic_get')
        import :: c_int, c_ptr
        type(c_ptr), value :: s, r
        integer(c_int), value :: n
    end subroutine
    function c_setbasic_insert(self, v) result(res) bind(c, name='setbasic_insert')
        import :: c_int, c_ptr
        type(c_ptr), value :: self, v
        integer(c_int) :: res
    end function
    function c_setbasic_size(s) result(res) bind(c, name='setbasic_size')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long) :: res
    end function
    function c_vecbasic_new() bind(c, name='vecbasic_new')
        import c_ptr
        type(c_ptr) :: c_vecbasic_new
    end function
    subroutine c_vecbasic_free(s) bind(c, name='vecbasic_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_vecbasic_push_back(s, val) bind(c, name='vecbasic_push_back')
        import :: c_ptr, c_long
        type(c_ptr), value :: val, s
        integer(c_long) :: c_vecbasic_push_back
    end function
    function c_dense_matrix_new() result(res) bind(c, name='dense_matrix_new')
        import :: c_ptr
        type(c_ptr) :: res
    end function
    function c_dense_matrix_new_vec(rows, cols, l) result(res) bind(c, name='dense_matrix_new_vec')
        import :: c_ptr, c_long
        integer(c_long), value :: rows, cols
        type(c_ptr), value :: l
        type(c_ptr) :: res
    end function
    subroutine c_dense_matrix_free(s) bind(c, name='dense_matrix_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_dense_matrix_set(s, d) result(res) bind(c, name='dense_matrix_set')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, d
        integer(c_long) :: res
    end function
    function c_dense_matrix_add_scalar(s, a, b) result(res) bind(c, name='dense_matrix_add_scalar')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_mul_scalar(s, a, b) result(res) bind(c, name='dense_matrix_mul_scalar')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_str(s) result(res) bind(c, name='dense_matrix_str')
        import :: c_ptr
        type(c_ptr), value :: s
        type(c_ptr) :: res
    end function
    function c_dense_matrix_eq(lhs, rhs) result(res) bind(c, name='dense_matrix_eq')
        import :: c_int, c_ptr
        type(c_ptr), value :: lhs, rhs
        integer(c_int) :: res
    end function
    function c_dense_matrix_transpose(s, a) result(res) bind(c, name='dense_matrix_transpose')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_dense_matrix_mul_matrix(s, a, b) result(res) bind(c, name='dense_matrix_mul_matrix')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_add_matrix(s, a, b) result(res) bind(c, name='dense_matrix_add_matrix')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_ones(s, r, c) result(res) bind(c, name='dense_matrix_ones')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: r, c
        integer(c_long) :: res
    end function
    function c_dense_matrix_zeros(s, r, c) result(res) bind(c, name='dense_matrix_zeros')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: r, c
        integer(c_long) :: res
    end function
    function c_dense_matrix_eye(s, n, m, k) result(res) bind(c, name='dense_matrix_eye')
        import :: c_long, c_int, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: n, m
        integer(c_int), value :: k
        integer(c_long) :: res
    end function
end interface



type Basic
    type(c_ptr) :: ptr = c_null_ptr
    logical :: tmp = .false.
contains
    procedure :: str, subs, evalf, basic_assign
    procedure :: basic_add, basic_sub, basic_neg, basic_mul
    procedure :: basic_div, basic_pow, basic_eq, basic_neq
    procedure, pass(this) :: free_symbols => basic_free_symbols
    procedure, pass(this) :: basic_add_i_left, basic_add_i_right, basic_add_i64_left, basic_add_i64_right
    procedure, pass(this) :: basic_add_f_left, basic_add_f_right, basic_add_d_left, basic_add_d_right
    procedure, pass(this) :: basic_sub_i_left, basic_sub_i_right, basic_sub_i64_left, basic_sub_i64_right
    procedure, pass(this) :: basic_sub_f_left, basic_sub_f_right, basic_sub_d_left, basic_sub_d_right
    procedure, pass(this) :: basic_mul_i_left, basic_mul_i_right, basic_mul_i64_left, basic_mul_i64_right
    procedure, pass(this) :: basic_mul_f_left, basic_mul_f_right, basic_mul_d_left, basic_mul_d_right
    procedure, pass(this) :: basic_div_i_left, basic_div_i_right, basic_div_i64_left, basic_div_i64_right
    procedure, pass(this) :: basic_div_f_left, basic_div_f_right, basic_div_d_left, basic_div_d_right
    procedure, pass(this) :: basic_pow_i_left, basic_pow_i_right, basic_pow_i64_left, basic_pow_i64_right
    procedure, pass(this) :: basic_pow_f_left, basic_pow_f_right, basic_pow_d_left, basic_pow_d_right
    generic :: assignment(=) => basic_assign
    generic :: operator(+) => basic_add
    generic :: operator(+) => basic_add_i_left, basic_add_i_right, basic_add_i64_left, basic_add_i64_right
    generic :: operator(+) => basic_add_f_left, basic_add_f_right, basic_add_d_left, basic_add_d_right
    generic :: operator(-) => basic_sub
    generic :: operator(-) => basic_sub_i_left, basic_sub_i_right, basic_sub_i64_left, basic_sub_i64_right
    generic :: operator(-) => basic_sub_f_left, basic_sub_f_right, basic_sub_d_left, basic_sub_d_right
    generic :: operator(-) => basic_neg
    generic :: operator(*) => basic_mul
    generic :: operator(*) => basic_mul_i_left, basic_mul_i_right, basic_mul_i64_left, basic_mul_i64_right
    generic :: operator(*) => basic_mul_f_left, basic_mul_f_right, basic_mul_d_left, basic_mul_d_right
    generic :: operator(/) => basic_div
    generic :: operator(/) => basic_div_i_left, basic_div_i_right, basic_div_i64_left, basic_div_i64_right
    generic :: operator(/) => basic_div_f_left, basic_div_f_right, basic_div_d_left, basic_div_d_right
    generic :: operator(**) => basic_pow
    generic :: operator(**) => basic_pow_i_left, basic_pow_i_right, basic_pow_i64_left, basic_pow_i64_right
    generic :: operator(**) => basic_pow_f_left, basic_pow_f_right, basic_pow_d_left, basic_pow_d_right
    generic :: operator(==) => basic_eq
    generic :: operator(/=) => basic_neq
    final :: basic_free
end type

interface Basic
   module procedure basic_new
end interface

interface sin
    module procedure basic_sin
end interface

interface cos
    module procedure basic_cos
end interface

interface exp
    module procedure basic_exp
end interface

interface log
    module procedure basic_log
end interface

interface abs
    module procedure basic_abs
end interface

interface sqrt
    module procedure basic_sqrt
end interface

interface atan2
    module procedure basic_atan2
    module procedure basic_atan2_i_left
    module procedure basic_atan2_i_right
    module procedure basic_atan2_i64_left
    module procedure basic_atan2_i64_right
    module procedure basic_atan2_f_left
    module procedure basic_atan2_f_right
    module procedure basic_atan2_d_left
    module procedure basic_atan2_d_right
end interface

interface max
    module procedure basic_max
end interface

type, extends(Basic) :: SymInteger
contains
    procedure :: get
end type SymInteger

interface SymInteger
    module procedure integer_new
    module procedure integer_new_i64
end interface

type, extends(Basic) :: Rational
end type Rational

interface Rational
    module procedure rational_new
end interface

type, extends(Basic) :: RealDouble
end type RealDouble

interface RealDouble
    module procedure real_new_d
    module procedure real_new_f
end interface

type, extends(Basic) :: SymComplex
end type SymComplex

interface SymComplex
    module procedure complex_new
    module procedure complex_new_i_i
    module procedure complex_new_i64_i
    module procedure complex_new_i_i64
    module procedure complex_new_i64_i64
end interface

type, extends(Basic) :: Symbol
end type Symbol

interface Symbol
    module procedure symbol_new
end interface


type SetBasic
    type(c_ptr) :: ptr = c_null_ptr
    logical :: tmp = .false.
contains
    procedure :: setbasic_assign
    procedure, pass(this) :: size => setbasic_size
    procedure, pass(this) :: get => setbasic_get
    generic :: assignment(=) => setbasic_assign
    final :: setbasic_free
end type

interface SetBasic
   module procedure setbasic_new
end interface


type DenseMatrix
    type(c_ptr) :: ptr = c_null_ptr
    logical :: tmp = .false.
contains
    procedure :: str => matrix_str
    procedure :: matrix_assign, matrix_eq
    procedure, pass(this) :: matrix_add_scalar_left, matrix_add_scalar_right
    procedure, pass(this) :: matrix_add_scalar_i_left, matrix_add_scalar_i_right
    procedure, pass(this) :: matrix_add_scalar_i64_left, matrix_add_scalar_i64_right
    procedure, pass(this) :: matrix_add_scalar_f_left, matrix_add_scalar_f_right
    procedure, pass(this) :: matrix_add_scalar_d_left, matrix_add_scalar_d_right
    procedure, pass(this) :: matrix_neg
    procedure, pass(this) :: matrix_sub_scalar_left, matrix_sub_scalar_right
    procedure, pass(this) :: matrix_sub_scalar_i_left, matrix_sub_scalar_i_right
    procedure, pass(this) :: matrix_sub_scalar_i64_left, matrix_sub_scalar_i64_right
    procedure, pass(this) :: matrix_sub_scalar_f_left, matrix_sub_scalar_f_right
    procedure, pass(this) :: matrix_sub_scalar_d_left, matrix_sub_scalar_d_right
    procedure, pass(this) :: matrix_mul_scalar_left, matrix_mul_scalar_right
    procedure, pass(this) :: matrix_mul_scalar_i_left, matrix_mul_scalar_i_right
    procedure, pass(this) :: matrix_mul_scalar_i64_left, matrix_mul_scalar_i64_right
    procedure, pass(this) :: matrix_mul_scalar_f_left, matrix_mul_scalar_f_right
    procedure, pass(this) :: matrix_mul_scalar_d_left, matrix_mul_scalar_d_right
    procedure :: matrix_mul_dense
    procedure :: matrix_add_dense
    procedure :: matrix_sub_dense
    procedure, pass(this) :: matrix_div_scalar_left
    procedure, pass(this) :: matrix_div_scalar_i_left
    procedure, pass(this) :: matrix_div_scalar_i64_left
    procedure, pass(this) :: matrix_div_scalar_f_left
    procedure, pass(this) :: matrix_div_scalar_d_left
    generic :: assignment(=) => matrix_assign
    generic :: operator(+) => matrix_add_scalar_left, matrix_add_scalar_right
    generic :: operator(+) => matrix_add_scalar_i_left, matrix_add_scalar_i_right
    generic :: operator(+) => matrix_add_scalar_i64_left, matrix_add_scalar_i64_right
    generic :: operator(+) => matrix_add_scalar_f_left, matrix_add_scalar_f_right
    generic :: operator(+) => matrix_add_scalar_d_left, matrix_add_scalar_d_right
    generic :: operator(+) => matrix_add_dense
    generic :: operator(-) => matrix_neg
    generic :: operator(-) => matrix_sub_scalar_left, matrix_sub_scalar_right
    generic :: operator(-) => matrix_sub_scalar_i_left, matrix_sub_scalar_i_right
    generic :: operator(-) => matrix_sub_scalar_i64_left, matrix_sub_scalar_i64_right
    generic :: operator(-) => matrix_sub_scalar_f_left, matrix_sub_scalar_f_right
    generic :: operator(-) => matrix_sub_scalar_d_left, matrix_sub_scalar_d_right
    generic :: operator(-) => matrix_sub_dense
    generic :: operator(*) => matrix_mul_scalar_left, matrix_mul_scalar_right
    generic :: operator(*) => matrix_mul_scalar_i_left, matrix_mul_scalar_i_right
    generic :: operator(*) => matrix_mul_scalar_i64_left, matrix_mul_scalar_i64_right
    generic :: operator(*) => matrix_mul_scalar_f_left, matrix_mul_scalar_f_right
    generic :: operator(*) => matrix_mul_scalar_d_left, matrix_mul_scalar_d_right
    generic :: operator(*) => matrix_mul_dense
    generic :: operator(/) => matrix_div_scalar_left
    generic :: operator(/) => matrix_div_scalar_i_left
    generic :: operator(/) => matrix_div_scalar_i64_left
    generic :: operator(/) => matrix_div_scalar_f_left
    generic :: operator(/) => matrix_div_scalar_d_left
    generic :: operator(==) => matrix_eq
    final :: matrix_free
end type

interface DenseMatrix
    module procedure matrix_new
end interface

interface transpose
    module procedure transpose_dense
end interface


private
public :: ptr
public :: Basic, SymInteger, Rational, RealDouble, Symbol, parse, sin, cos, exp, log, abs, sqrt, atan2, max, SymComplex
public :: pi, e, eulergamma, catalan, goldenratio
public :: SetBasic
public :: DenseMatrix, transpose, ones, zeros, eye

contains

function ptr(a) result(res)
    class(Basic) :: a
    type(c_ptr) :: res
    res = a%ptr
end function

function setbasic_new() result(new)
    type(SetBasic) :: new
    new%ptr = c_setbasic_new()
end function

subroutine setbasic_free(this)
    type(SetBasic) :: this
    call c_setbasic_free(this%ptr)
end subroutine

subroutine setbasic_assign(a, b)
    class(SetBasic), intent(inout) :: a
    class(SetBasic), intent(in) :: b
    integer(c_long) :: exception
    integer :: i, dummy
    type(Basic) :: temp
    if (c_associated(a%ptr)) then
        call setbasic_free(a)
    end if
    a%ptr = c_setbasic_new()
    do i = 1, b%size()
        temp = b%get(i)
        dummy = c_setbasic_insert(a%ptr, temp%ptr)
    end do
    if (b%tmp) then
        call setbasic_free(b)
    end if
end subroutine

function setbasic_size(this) result(res)
    class(SetBasic) :: this
    integer :: res
    res = int(c_setbasic_size(this%ptr))
end function

function setbasic_get(this, n) result(res)
    class(SetBasic) :: this
    integer :: n
    type(Basic) :: res
    res = Basic()
    call c_setbasic_get(this%ptr, n - 1, res%ptr)
    res%tmp = .true.
end function

function basic_new() result(new)
    type(Basic) :: new
    new%ptr = c_basic_new_heap()
end function

subroutine basic_free(this)
    type(Basic) :: this
    call c_basic_free_heap(this%ptr)
end subroutine

function str(e)
    class(Basic) :: e
    character, pointer, dimension(:) :: tempstr
    character(:), allocatable :: str
    type(c_ptr) :: cstring
    integer :: nchars
    cstring = c_basic_str(e%ptr)
    nchars = c_strlen(cstring)
    call c_f_pointer(cstring, tempstr, [nchars])
    allocate(character(len=nchars) :: str)
    str = transfer(tempstr(1:nchars), str)
    call c_basic_str_free(cstring)
end function

function subs(e, a, b)
    class(Basic) :: e, a, b
    integer(c_long) :: exception
    type(Basic) :: subs
    subs = Basic()
    exception = c_basic_subs2(subs%ptr, e%ptr, a%ptr, b%ptr) 
    call handle_exception(exception)
    subs%tmp = .true.
end function

function basic_free_symbols(this) result(res)
    class(Basic) :: this
    type(SetBasic) :: res
    integer(c_long) :: exception
    res = SetBasic()
    exception = c_basic_free_symbols(this%ptr, res%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

subroutine basic_assign(a, b)
    class(basic), intent(inout) :: a
    class(basic), intent(in) :: b
    integer(c_long) :: exception
    if (.not. c_associated(a%ptr)) then
        a%ptr = c_basic_new_heap()
    end if
    exception = c_basic_assign(a%ptr, b%ptr)
    call handle_exception(exception)
    if (b%tmp) then
        call basic_free(b)
    end if
end subroutine

function basic_add(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_add(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function basic_add_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_f_left(this, b)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: basic_add_f_left
    basic_add_f_left = basic_add(this, RealDouble(b))
end function

function basic_add_f_right(b, this)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: basic_add_f_right
    basic_add_f_right = basic_add(this, RealDouble(b))
end function

function basic_add_d_left(this, b)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: basic_add_d_left
    basic_add_d_left = basic_add(this, RealDouble(b))
end function

function basic_add_d_right(b, this)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: basic_add_d_right
    basic_add_d_right = basic_add(this, RealDouble(b))
end function

function basic_sub(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_sub
    integer(c_long) :: exception
    basic_sub = Basic()
    exception = c_basic_sub(basic_sub%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    basic_sub%tmp = .true.
end function

function basic_sub_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, SymInteger(b))
end function

function basic_sub_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(SymInteger(b), this)
end function

function basic_sub_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, SymInteger(b))
end function

function basic_sub_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(SymInteger(b), this)
end function

function basic_sub_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, RealDouble(b))
end function

function basic_sub_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(RealDouble(b), this)
end function

function basic_sub_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, RealDouble(b))
end function

function basic_sub_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(RealDouble(b), this)
end function

function basic_neg(a)
    class(basic), intent(in) :: a
    type(basic) :: basic_neg, zero
    integer(c_long) :: exception
    zero = SymInteger(0)
    basic_neg = Basic()
    exception = c_basic_sub(basic_neg%ptr, zero%ptr, a%ptr)
    call handle_exception(exception)
    basic_neg%tmp = .true.
end function

function basic_mul(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_mul
    integer(c_long) :: exception
    basic_mul = Basic()
    exception = c_basic_mul(basic_mul%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    basic_mul%tmp = .true.
end function

function basic_mul_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, SymInteger(b))
end function

function basic_mul_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(SymInteger(b), this)
end function

function basic_mul_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, SymInteger(b))
end function

function basic_mul_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(SymInteger(b), this)
end function

function basic_mul_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, RealDouble(b))
end function

function basic_mul_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(RealDouble(b), this)
end function

function basic_mul_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, RealDouble(b))
end function

function basic_mul_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(RealDouble(b), this)
end function

function basic_div(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_div
    integer(c_long) :: exception
    basic_div = Basic()
    exception = c_basic_div(basic_div%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    basic_div%tmp = .true.
end function

function basic_div_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, SymInteger(b))
end function

function basic_div_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_div(SymInteger(b), this)
end function

function basic_div_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, SymInteger(b))
end function

function basic_div_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_div(SymInteger(b), this)
end function

function basic_div_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, RealDouble(b))
end function

function basic_div_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_div(RealDouble(b), this)
end function

function basic_div_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, RealDouble(b))
end function

function basic_div_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_div(RealDouble(b), this)
end function

function basic_pow(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_pow
    integer(c_long) :: exception
    basic_pow = Basic()
    exception = c_basic_pow(basic_pow%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    basic_pow%tmp = .true.
end function

function basic_pow_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, SymInteger(b))
end function

function basic_pow_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(SymInteger(b), this)
end function

function basic_pow_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, SymInteger(b))
end function

function basic_pow_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(SymInteger(b), this)
end function

function basic_pow_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, RealDouble(b))
end function

function basic_pow_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(RealDouble(b), this)
end function

function basic_pow_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, RealDouble(b))
end function

function basic_pow_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(RealDouble(b), this)
end function

function basic_eq(a, b)
    class(basic), intent(in) :: a, b
    logical :: basic_eq
    integer(c_int) :: dummy
    dummy = c_basic_eq(a%ptr, b%ptr)
    basic_eq = (dummy /= 0)
end function

function basic_neq(a, b)
    class(basic), intent(in) :: a, b
    logical :: basic_neq
    integer(c_int) :: dummy
    dummy = c_basic_neq(a%ptr, b%ptr)
    basic_neq = (dummy /= 0)
end function

function basic_sin(a)
    class(basic), intent(in) :: a
    type(basic) :: basic_sin
    integer(c_long) :: exception
    basic_sin = Basic()
    exception = c_basic_sin(basic_sin%ptr, a%ptr)
    call handle_exception(exception)
    basic_sin%tmp = .true.
end function

function basic_cos(a)
    class(basic), intent(in) :: a
    type(basic) :: basic_cos
    integer(c_long) :: exception
    basic_cos = Basic()
    exception = c_basic_cos(basic_cos%ptr, a%ptr)
    call handle_exception(exception)
    basic_cos%tmp = .true.
end function

function basic_exp(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_exp(res%ptr, a%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function basic_log(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_log(res%ptr, a%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function basic_abs(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_abs(res%ptr, a%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function basic_sqrt(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_sqrt(res%ptr, a%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function basic_atan2(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_atan2(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function basic_atan2_i_left(a, b) result(res)
    class(basic), intent(in) :: a
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = SymInteger(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_i_right(a, b) result(res)
    integer(kind=int32), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = SymInteger(a)
    res = basic_atan2(temp, b)
end function

function basic_atan2_i64_left(a, b) result(res)
    class(basic), intent(in) :: a
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = SymInteger(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_i64_right(a, b) result(res)
    integer(kind=int64), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = SymInteger(a)
    res = basic_atan2(temp, b)
end function

function basic_atan2_f_left(a, b) result(res)
    class(basic), intent(in) :: a
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = RealDouble(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_f_right(a, b) result(res)
    real(kind=real32), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = RealDouble(a)
    res = basic_atan2(temp, b)
end function

function basic_atan2_d_left(a, b) result(res)
    class(basic), intent(in) :: a
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = RealDouble(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_d_right(a, b) result(res)
    real(kind=real64), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp
    temp = RealDouble(a)
    res = basic_atan2(temp, b)
end function

function pi()
    type(basic) :: pi
    pi = Basic()
    call c_basic_const_pi(pi%ptr)
    pi%tmp = .true.
end function

function e()
    type(basic) :: e
    e = Basic()
    call c_basic_const_e(e%ptr)
    e%tmp = .true.
end function

function eulergamma() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_const_euler_gamma(res%ptr)
    res%tmp = .true.
end function

function catalan() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_const_catalan(res%ptr)
    res%tmp = .true.
end function

function goldenratio() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_const_goldenratio(res%ptr)
    res%tmp = .true.
end function

function evalf(b, bits, r)
    class(basic), intent(in) :: b
    integer(c_long) :: bits
    integer(c_int) :: r
    type(basic) :: evalf
    integer(c_long) :: exception
    evalf = Basic()
    exception = c_basic_evalf(evalf%ptr, b%ptr, bits, r)
    call handle_exception(exception)
    evalf%tmp = .true.
end function

function integer_new(i)
    integer :: i
    integer(c_long) :: j
    integer(c_long) :: exception
    type(SymInteger) :: integer_new
    j = int(i)
    integer_new%ptr = c_basic_new_heap()
    exception = c_integer_set_si(integer_new%ptr, j)
    call handle_exception(exception)
    integer_new%tmp = .true.
end function

function integer_new_i64(i)
    integer(kind=int64) :: i
    integer(c_long) :: j
    integer(c_long) :: exception
    type(SymInteger) :: integer_new_i64
    j = int(i)
    integer_new_i64%ptr = c_basic_new_heap()
    exception = c_integer_set_si(integer_new_i64%ptr, j)
    call handle_exception(exception)
    integer_new_i64%tmp = .true.
end function

function get(this) result(i)
    class(SymInteger) :: this
    integer :: i
    i = int(c_integer_get_si(this%ptr))
end function

function rational_new(a, b)
    integer :: a, b
    integer(c_long) :: x, y
    integer(c_long) :: exception
    type(Rational) :: rational_new
    x = int(a)
    y = int(b)
    rational_new%ptr = c_basic_new_heap()
    exception = c_rational_set_si(rational_new%ptr, x, y)
    call handle_exception(exception)
    rational_new%tmp = .true.
end function

function real_new_d(d)
    real(c_double) :: d
    integer(c_long) :: exception
    type(RealDouble) :: real_new_d
    real_new_d%ptr = c_basic_new_heap()
    exception = c_real_double_set_d(real_new_d%ptr, d)
    call handle_exception(exception)
    real_new_d%tmp = .true.
end function

function real_new_f(f)
    real :: f
    integer(c_long) :: exception
    type(RealDouble) :: real_new_f
    real_new_f%ptr = c_basic_new_heap()
    exception = c_real_double_set_d(real_new_f%ptr, real(f, 8))
    call handle_exception(exception)
    real_new_f%tmp = .true.
end function

function complex_new(re, im) result(res)
    class(basic) :: re, im
    type(basic) :: res
    integer(c_long) :: exception
    res%ptr = c_basic_new_heap()
    exception = c_complex_set(res%ptr, re%ptr, im%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function complex_new_i_i(re, im) result(res)
    integer(kind=int32) :: re, im
    type(basic) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function complex_new_i_i64(re, im) result(res)
    integer(kind=int32) :: re
    integer(kind=int64) :: im
    type(basic) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function complex_new_i64_i(re, im) result(res)
    integer(kind=int64) :: re
    integer(kind=int32) :: im
    type(basic) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function complex_new_i64_i64(re, im) result(res)
    integer(kind=int64) :: re, im
    type(SymComplex) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function symbol_new(c)
    character(len=*) :: c
    character(len=len_trim(c) + 1) :: new_c
    integer(c_long) :: exception
    type(Symbol) :: symbol_new
    new_c = trim(c) // c_null_char
    symbol_new%ptr = c_basic_new_heap()
    symbol_new%tmp = .true.
    exception = c_symbol_set(symbol_new%ptr, new_c) 
    call handle_exception(exception)
end function

function parse(c)
    character(len=*) :: c
    type(Basic) :: parse
    integer(c_long) :: exception
    character(len=len_trim(c) + 1) :: new_c
    new_c = trim(c) // c_null_char
    parse%ptr = c_basic_new_heap()
    exception = c_basic_parse(parse%ptr, new_c) 
    call handle_exception(exception)
    parse%tmp = .true.
end function

function basic_max(d)
    type(c_ptr), dimension(:) :: d
    type(Basic) :: basic_max
    integer :: i
    type(c_ptr) :: vec
    integer(c_long) :: exception
    vec = c_vecbasic_new()

    do i = 1, size(d)
        exception = c_vecbasic_push_back(vec, d(i))
        call handle_exception(exception)
    end do

    basic_max = Basic()
    exception = c_basic_max(basic_max%ptr, vec)
    call c_vecbasic_free(vec)
    call handle_exception(exception)
    basic_max%tmp = .true.
end function

function matrix_new(rows, cols, d) result(res)
    integer :: rows, cols
    type(c_ptr), dimension(:) :: d
    type(DenseMatrix) :: res

    type(c_ptr) :: vec
    integer(c_long) :: exception, rlong, clong
    integer :: i

    vec = c_vecbasic_new()

    do i = 1, size(d)
        exception = c_vecbasic_push_back(vec, d(i))
        call handle_exception(exception)
    end do

    rlong = int(rows)
    clong = int(cols)
    res%ptr = c_dense_matrix_new_vec(rlong, clong, vec)
    call c_vecbasic_free(vec)
    res%tmp = .true.
end function

subroutine matrix_free(this)
    type(DenseMatrix) :: this
    call c_dense_matrix_free(this%ptr)
end subroutine

subroutine matrix_assign(a, b)
    class(DenseMatrix), intent(inout) :: a
    class(DenseMatrix), intent(in) :: b
    integer(c_long) :: exception
    if (.not. c_associated(a%ptr)) then
        a%ptr = c_dense_matrix_new()
    end if
    exception = c_dense_matrix_set(a%ptr, b%ptr)
    call handle_exception(exception)
    if (b%tmp) then
        call matrix_free(b)
    end if
end subroutine

function matrix_add_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_add_scalar(res%ptr, this%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function matrix_add_scalar_right(a, this) result(res)
    class(Basic), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, a)
end function

function matrix_add_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_i_right(a, this) result(res)
    integer(kind=int32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_i_left(this, a)
end function

function matrix_add_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_i64_right(a, this) result(res)
    integer(kind=int64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_i64_left(this, a)
end function

function matrix_add_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_f_right(a, this) result(res)
    real(kind=real32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_f_left(this, a)
end function

function matrix_add_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_d_right(a, this) result(res)
    real(kind=real64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_d_left(this, a)
end function

function matrix_str(e) result(res)
    class(DenseMatrix) :: e
    character, pointer, dimension(:) :: tempstr
    character(:), allocatable :: res
    type(c_ptr) :: cstring
    integer :: nchars
    cstring = c_dense_matrix_str(e%ptr)
    nchars = c_strlen(cstring)
    call c_f_pointer(cstring, tempstr, [nchars])
    allocate(character(len=nchars) :: res)
    res = transfer(tempstr(1:nchars), res)
    call c_basic_str_free(cstring)
end function

function matrix_eq(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    logical :: res
    integer(c_int) :: dummy
    dummy = c_dense_matrix_eq(a%ptr, b%ptr)
    res = (dummy /= 0)
end function

function matrix_neg(this) result(res)
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    type(Basic) :: m1
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    m1 = SymInteger(-1)
    exception = c_dense_matrix_mul_scalar(res%ptr, this%ptr, m1%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function matrix_sub_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, -b)
end function

function matrix_sub_scalar_right(a, this) result(res)
    class(Basic), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, a)
end function

function matrix_sub_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, SymInteger(-b))
end function

function matrix_sub_scalar_i_right(a, this) result(res)
    integer(kind=int32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, SymInteger(a))
end function

function matrix_sub_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, SymInteger(-b))
end function

function matrix_sub_scalar_i64_right(a, this) result(res)
    integer(kind=int64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, SymInteger(a))
end function

function matrix_sub_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, RealDouble(-b))
end function

function matrix_sub_scalar_f_right(a, this) result(res)
    real(kind=real32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, RealDouble(a))
end function

function matrix_sub_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, RealDouble(-b))
end function

function matrix_sub_scalar_d_right(a, this) result(res)
    real(kind=real64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, RealDouble(a))
end function

function matrix_mul_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_mul_scalar(res%ptr, this%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function matrix_mul_scalar_right(a, this) result(res)
    class(Basic), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_left(this, a)
end function

function matrix_mul_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_i_right(a, this) result(res)
    integer(kind=int32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_i_left(this, a)
end function

function matrix_mul_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_i64_right(a, this) result(res)
    integer(kind=int64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_i64_left(this, a)
end function

function matrix_mul_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_f_right(a, this) result(res)
    real(kind=real32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_f_left(this, a)
end function

function matrix_mul_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_d_right(a, this) result(res)
    real(kind=real64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_d_left(this, a)
end function

function matrix_div_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    type(Basic) :: inverse
    integer(c_long) :: exception
    inverse = 1 / b
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_mul_scalar(res%ptr, this%ptr, inverse%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function matrix_div_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_div_scalar_left(this, i)
end function

function matrix_div_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_div_scalar_left(this, i)
end function

function matrix_div_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_div_scalar_left(this, i)
end function

function matrix_div_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_div_scalar_left(this, i)
end function

function transpose_dense(a) result(res)
    class(DenseMatrix), intent(in) :: a
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_transpose(res%ptr, a%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function matrix_mul_dense(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_mul_matrix(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function matrix_add_dense(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_add_matrix(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function matrix_sub_dense(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    type(DenseMatrix) :: neg
    neg = -b
    res = matrix_add_dense(a, neg)
end function

function ones(r, c) result(res)
    integer(kind=int32) :: r, c
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_ones(res%ptr, int(r, 8), int(c, 8))
    call handle_exception(exception)
    res%tmp = .true.
end function

function zeros(r, c) result(res)
    integer(kind=int32) :: r, c
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_zeros(res%ptr, int(r, 8), int(c, 8))
    call handle_exception(exception)
    res%tmp = .true.
end function

function eye(n, k) result(res)
    integer(kind=int32) :: n
    integer(kind=int32) :: k
    type(DenseMatrix) :: res
    integer(c_long) :: exception
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_eye(res%ptr, int(n, 8), int(n, 8), k)
    call handle_exception(exception)
    res%tmp = .true.
end function


end module
