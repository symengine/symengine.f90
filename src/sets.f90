module sets

use symengine_interface
use symengine_basic

implicit none

private
public :: emptyset

contains

function emptyset() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_set_emptyset(res%ptr)
    res%tmp = .true.
end function

end module
