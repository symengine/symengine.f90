module small_test

use symengine_cwrapper
use iso_c_binding

implicit none

contains

  function c_char_ptr_to_fstring(c_char_ptr) result(fc)
    type(c_ptr) :: c_char_ptr
    character(len=:,kind=c_char), allocatable :: fc
    character(len=1000,kind=c_char), pointer :: f_string
    
    if (.not. c_associated(c_char_ptr)) then
        fc = ""
    else
        call c_f_pointer(c_char_ptr,f_string)
        fc = f_string(1:index(f_string,c_null_char))
    end if
  end function

subroutine f(i)

  integer(c_long), intent(in) :: i

  type(c_ptr) :: s = c_null_ptr ! string

  type(c_ptr) :: x = c_null_ptr
  type(c_ptr) :: y = c_null_ptr
  type(c_ptr) :: e = c_null_ptr
  type(c_ptr) :: n = c_null_ptr

  type(c_ptr) :: exception = c_null_ptr

  write(*,'(A)') c_char_ptr_to_fstring(ascii_art_str())
  write(*,'(A)') "Symengine version: "//c_char_ptr_to_fstring(symengine_version())
  write(*,*)

  call basic_new_stack(x)
  call basic_new_stack(y)
  call basic_new_stack(e)
  call basic_new_stack(n)

  exception = symbol_set(x,"x"//c_null_char)
  exception = symbol_set(y,"y"//c_null_char)

  exception = integer_set_si(n, i);
  exception = basic_mul(e, n, x);
  exception = basic_add(e, e, y);

  s = basic_str(e)
  print *, "Result: ", c_char_ptr_to_fstring(s)
  call basic_str_free(s)
  s = c_null_ptr

  print *, c_associated(s), c_char_ptr_to_fstring(s)

  exception = basic_parse(e,"3*x**2 + 4*y + 5*a"//c_null_char)
  s = basic_str(e)
  print *, "Result: ", c_char_ptr_to_fstring(s), c_associated(exception)
  exception = basic_diff(s=e,expr=e,sym=x)
  s = basic_str(e)
  print *, "Result: ", c_char_ptr_to_fstring(s), c_associated(exception)

  call basic_free_stack(x)
  call basic_free_stack(y)
  call basic_free_stack(e)
  call basic_free_stack(n)

end subroutine

end module

program small_test_driver

  use small_test
  use iso_c_binding
  use symengine

  implicit none

  type(basic) :: a, b

  call f(5_c_long)

  call a%zero()
  call b%zero()

  print *, a == b

end program