module functions

use iso_fortran_env, only: int32, int64, real32, real64
use symengine_interface
use symengine_basic
use exceptions 
implicit none

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


private
public :: sin, cos, exp, log, abs, sqrt, atan2, max

contains

function basic_sin(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_sin(res%ptr, a%ptr)
    call handle_exception(exception)
    res%tmp = .true.
end function

function basic_cos(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_cos(res%ptr, a%ptr)
    call handle_exception(exception)
    res%tmp = .true.
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

function basic_max(d) result(res)
    type(c_ptr), dimension(:) :: d
    type(Basic) :: res
    integer :: i
    type(c_ptr) :: vec
    integer(c_long) :: exception
    vec = c_vecbasic_new()

    do i = 1, size(d)
        exception = c_vecbasic_push_back(vec, d(i))
        call handle_exception(exception)
    end do

    res = Basic()
    exception = c_basic_max(res%ptr, vec)
    call c_vecbasic_free(vec)
    call handle_exception(exception)
    res%tmp = .true.
end function

end module
