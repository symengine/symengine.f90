module symengine_basic

use iso_c_binding, only: c_int, c_long, c_double, c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_associated
use iso_fortran_env, only: int32, int64, real32, real64
use symengine_interface
use exceptions


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

type, extends(Basic) :: SymInteger
contains
    procedure :: get
end type SymInteger

interface SymInteger
    module procedure integer_new
    module procedure integer_new_i64
end interface

type, extends(Basic) :: RealDouble
end type RealDouble

interface RealDouble
    module procedure real_new_d
    module procedure real_new_f
end interface



private
public :: ptr, Basic, SetBasic, SymInteger, RealDouble

contains

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

function str(e) result(res)
    class(Basic) :: e
    character, pointer, dimension(:) :: tempstr
    character(:), allocatable :: res
    type(c_ptr) :: cstring
    integer :: nchars
    cstring = c_basic_str(e%ptr)
    res = convert_string(cstring)
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

end module