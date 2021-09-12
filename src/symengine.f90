module symengine

use iso_c_binding, only: c_size_t, c_int, c_long, c_double, c_char, c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_associated
use iso_fortran_env, only: int64, real64
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
    function c_basic_sqrt(s, a) bind(c, name='basic_sqrt')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: c_basic_sqrt
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
    function c_symbol_set(s, c) bind(c, name='symbol_set')
        import c_long, c_ptr, c_char
        type(c_ptr), value :: s
        character(kind=c_char), dimension(*) :: c
        integer(c_long) :: c_symbol_set
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
        import c_ptr, c_long
        type(c_ptr), value :: val, s
        integer(c_long) :: c_vecbasic_push_back
    end function
end interface


type Basic
    type(c_ptr) :: ptr = c_null_ptr
    logical :: tmp = .false.
contains
    procedure :: str, subs, evalf, basic_assign, basic_add
    procedure :: basic_sub, basic_neg, basic_mul
    procedure :: basic_div, basic_pow, basic_eq, basic_neq
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

interface sqrt
    module procedure basic_sqrt
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

type, extends(Basic) :: Symbol
end type Symbol

interface Symbol
    module procedure symbol_new
end interface

private
public :: ptr
public :: Basic, SymInteger, Rational, RealDouble, Symbol, parse, sin, cos, sqrt, max, pi, e


contains

function ptr(a)
    class(Basic) :: a
    type(c_ptr) :: ptr
    ptr = a%ptr
end function

subroutine handle_exception(a)
    integer(c_long) :: a
    if (a == 1) then
        error stop "Runtime error"
    else if (a == 2) then
        error stop "Division by zero"
    else if (a == 3) then
        error stop "Not implemented"
    else if (a == 4) then
        error stop "Domain error"
    else if (a == 5) then
        error stop "Parse error"
    end if
end subroutine

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
    integer, intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer, intent(in) :: b
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
    real, intent(in) :: b
    type(basic) :: basic_add_f_left
    basic_add_f_left = basic_add(this, RealDouble(b))
end function

function basic_add_f_right(b, this)
    class(basic), intent(in) :: this
    real, intent(in) :: b
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
    integer, intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, SymInteger(b))
end function

function basic_sub_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer, intent(in) :: b
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
    real, intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, RealDouble(b))
end function

function basic_sub_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real, intent(in) :: b
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
    integer, intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, SymInteger(b))
end function

function basic_mul_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer, intent(in) :: b
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
    real, intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, RealDouble(b))
end function

function basic_mul_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real, intent(in) :: b
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
    integer, intent(in) :: b
    type(basic) :: res
    res = basic_div(this, SymInteger(b))
end function

function basic_div_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer, intent(in) :: b
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
    real, intent(in) :: b
    type(basic) :: res
    res = basic_div(this, RealDouble(b))
end function

function basic_div_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real, intent(in) :: b
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
    integer, intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, SymInteger(b))
end function

function basic_pow_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer, intent(in) :: b
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
    real, intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, RealDouble(b))
end function

function basic_pow_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real, intent(in) :: b
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

function basic_sqrt(a)
    class(basic), intent(in) :: a
    type(basic) :: basic_sqrt
    integer(c_long) :: exception
    basic_sqrt = Basic()
    exception = c_basic_sqrt(basic_sqrt%ptr, a%ptr)
    call handle_exception(exception)
    basic_sqrt%tmp = .true.
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

end module
