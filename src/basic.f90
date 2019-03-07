module basic
use iso_c_binding, only: c_int
implicit none

interface
    subroutine f_c(i) bind(c, name="f")
    import :: c_int
    integer(c_int), value, intent(in) :: i
    end subroutine
end interface

contains

subroutine f(i)
integer, intent(in) :: i
call f_c(i)
end subroutine

end module
