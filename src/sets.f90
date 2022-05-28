module sets

use symengine_interface
use symengine_basic
use exceptions

implicit none

private
public :: emptyset, universalset, complexes, reals, rationals, integers, set_union, set_intersection

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
