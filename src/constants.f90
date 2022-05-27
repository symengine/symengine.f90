module constants

use symengine_interface
use symengine_basic

implicit none

contains

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


end module
