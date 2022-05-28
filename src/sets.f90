module sets

use iso_fortran_env, only: int32, int64, real32, real64

use symengine_interface
use symengine_basic
use exceptions

implicit none

interface interval
    module procedure basic_interval
    module procedure basic_interval_i_i, basic_interval_i_i64, basic_interval_i64_i, basic_interval_i64_i64
    module procedure basic_interval_f_f, basic_interval_f_d, basic_interval_d_f, basic_interval_d_d
    module procedure basic_interval_f_i, basic_interval_i_f, basic_interval_f_i64, basic_interval_i64_f
    module procedure basic_interval_d_i, basic_interval_i_d, basic_interval_d_i64, basic_interval_i64_d
end interface

private
public :: emptyset, universalset, complexes, reals, rationals, integers, set_union, set_intersection
public :: interval

contains

type(basic) function emptyset() result(res)
    res = Basic()
    call c_basic_set_emptyset(res%ptr)
    res%tmp = .true.
end function

type(basic) function universalset() result(res)
    res = Basic()
    call c_basic_set_universalset(res%ptr)
    res%tmp = .true.
end function

type(basic) function complexes() result(res)
    res = Basic()
    call c_basic_set_complexes(res%ptr)
    res%tmp = .true.
end function

type(basic) function reals() result(res)
    res = Basic()
    call c_basic_set_reals(res%ptr)
    res%tmp = .true.
end function

type(basic) function rationals() result(res)
    res = Basic()
    call c_basic_set_rationals(res%ptr)
    res%tmp = .true.
end function

type(basic) function integers() result(res)
    res = Basic()
    call c_basic_set_integers(res%ptr)
    res%tmp = .true.
end function

type(basic) function basic_interval(a, b, left_open, right_open) result(res)
    class(basic), intent(in) :: a, b
    logical, optional :: left_open, right_open
    integer(c_int) :: a_left = 1, a_right = 1
    integer(c_long) :: exception
    if (present(left_open) .and. .not. left_open) then
        a_left = 0
    end if
    if (present(right_open) .and. .not. right_open) then
        a_right = 0
    end if
    res = Basic()
    exception = c_basic_set_interval(res%ptr, a%ptr, b%ptr, a_left, a_right)
    call handle_exception(exception)
    res%tmp = .true.
end function

type(basic) function basic_interval_i_i(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i_i64(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a
    integer(kind=int64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_i(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a
    integer(kind=int32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_i64(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_f_f(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_f_d(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a
    real(kind=real64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_f(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a
    real(kind=real32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_d(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_f_i(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a
    integer(kind=int32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i_f(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a
    real(kind=real32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_f_i64(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a
    integer(kind=int64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_f(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a
    real(kind=real32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_i(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a
    integer(kind=int32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i_d(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a
    real(kind=real64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_i64(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a
    integer(kind=int64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_d(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a
    real(kind=real64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

type(basic) function set_union(a, b) result(res)
    class(basic), intent(in) :: a, b
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_set_union(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

type(basic) function set_intersection(a, b) result(res)
    class(basic), intent(in) :: a, b
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_set_intersection(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

end module
