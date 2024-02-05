module constants

use symengine_interface
use symengine_basic

implicit none

contains

function pi()
    type(basic), allocatable :: pi
    allocate(pi)
    pi = Basic()
    call c_basic_const_pi(pi%ptr)
end function

function e()
    type(basic), allocatable :: e
    allocate(e)
    e = Basic()
    call c_basic_const_e(e%ptr)
end function

function eulergamma() result(res)
    type(basic), allocatable :: res
    allocate(res)
    res = Basic()
    call c_basic_const_euler_gamma(res%ptr)
end function

function catalan() result(res)
    type(basic), allocatable :: res
    allocate(res)
    res = Basic()
    call c_basic_const_catalan(res%ptr)
end function

function goldenratio() result(res)
    type(basic), allocatable :: res
    allocate(res)
    res = Basic()
    call c_basic_const_goldenratio(res%ptr)
end function


end module
