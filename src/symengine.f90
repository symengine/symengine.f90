module symengine

use iso_c_binding, only: c_size_t, c_int, c_long, c_char, c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_associated
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
    function c_symbol_set(s, c) bind(c, name='symbol_set')
        import c_long, c_ptr, c_char
        type(c_ptr), value :: s
        character(kind=c_char), dimension(*) :: c
        integer(c_long) :: c_symbol_set
    end function
end interface


type Basic
    type(c_ptr) :: ptr = c_null_ptr
    logical :: tmp = .false.
contains
    procedure :: str, basic_assign, basic_add, basic_sub, basic_mul, basic_div, basic_pow, basic_eq, basic_neq
    generic :: assignment(=) => basic_assign
    generic :: operator(+) => basic_add
    generic :: operator(-) => basic_sub
    generic :: operator(*) => basic_mul
    generic :: operator(/) => basic_div
    generic :: operator(**) => basic_pow
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

type, extends(Basic) :: SymInteger
contains
    procedure :: get
end type SymInteger

interface SymInteger
    module procedure integer_new
end interface

type, extends(Basic) :: Rational
end type Rational

interface Rational
    module procedure rational_new
end interface

type, extends(Basic) :: Symbol
end type Symbol

interface Symbol
    module procedure symbol_new
end interface

private
public :: Basic, SymInteger, Rational, Symbol, parse, sin, cos


contains


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

subroutine basic_assign(a, b)
    class(basic), intent(inout) :: a
    class(basic), intent(in) :: b
    integer(c_long) :: dummy
    if (.not. c_associated(a%ptr)) then
        a%ptr = c_basic_new_heap()
    end if
    dummy = c_basic_assign(a%ptr, b%ptr)
    if (b%tmp) then
        call basic_free(b)
    end if
end subroutine

function basic_add(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_add
    integer(c_long) :: dummy
    basic_add = Basic()
    dummy = c_basic_add(basic_add%ptr, a%ptr, b%ptr)
    basic_add%tmp = .true.
end function

function basic_sub(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_sub
    integer(c_long) :: dummy
    basic_sub = Basic()
    dummy = c_basic_sub(basic_sub%ptr, a%ptr, b%ptr)
    basic_sub%tmp = .true.
end function

function basic_mul(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_mul
    integer(c_long) :: dummy
    basic_mul = Basic()
    dummy = c_basic_mul(basic_mul%ptr, a%ptr, b%ptr)
    basic_mul%tmp = .true.
end function

function basic_div(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_div
    integer(c_long) :: dummy
    basic_div = Basic()
    dummy = c_basic_div(basic_div%ptr, a%ptr, b%ptr)
    basic_div%tmp = .true.
end function

function basic_pow(a, b)
    class(basic), intent(in) :: a, b
    type(basic) :: basic_pow
    integer(c_long) :: dummy
    basic_pow = Basic()
    dummy = c_basic_pow(basic_pow%ptr, a%ptr, b%ptr)
    basic_pow%tmp = .true.
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
    integer(c_long) :: dummy
    basic_sin = Basic()
    dummy = c_basic_sin(basic_sin%ptr, a%ptr)
    basic_sin%tmp = .true.
end function

function basic_cos(a)
    class(basic), intent(in) :: a
    type(basic) :: basic_cos
    integer(c_long) :: dummy
    basic_cos = Basic()
    dummy = c_basic_cos(basic_cos%ptr, a%ptr)
    basic_cos%tmp = .true.
end function

function integer_new(i)
    integer :: i
    integer(c_long) :: j
    integer(c_long) :: dummy
    type(SymInteger) :: integer_new
    j = int(i)
    integer_new%ptr = c_basic_new_heap()
    dummy = c_integer_set_si(integer_new%ptr, j)
    integer_new%tmp = .true.
end function

function get(this) result(i)
    class(SymInteger) :: this
    integer :: i
    i = int(c_integer_get_si(this%ptr))
end function

function rational_new(a, b)
    integer :: a, b
    integer(c_long) :: x, y
    integer(c_long) :: dummy
    type(Rational) :: rational_new
    x = int(a)
    y = int(b)
    rational_new%ptr = c_basic_new_heap()
    dummy = c_rational_set_si(rational_new%ptr, x, y)
    rational_new%tmp = .true.
end function

function symbol_new(c)
    character(len=*) :: c
    character(len=len_trim(c) + 1) :: new_c
    integer(c_long) :: dummy
    type(Symbol) :: symbol_new
    new_c = trim(c) // c_null_char
    symbol_new%ptr = c_basic_new_heap()
    symbol_new%tmp = .true.
    dummy = c_symbol_set(symbol_new%ptr, new_c) 
end function

function parse(c)
    character(len=*) :: c
    type(Basic) :: parse
    integer(c_long) :: dummy
    character(len=len_trim(c) + 1) :: new_c
    new_c = trim(c) // c_null_char
    parse%ptr = c_basic_new_heap()
    dummy = c_basic_parse(parse%ptr, new_c) 
    parse%tmp = .true.
end function

end module
