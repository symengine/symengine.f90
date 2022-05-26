module symengine_interface

use iso_c_binding, only: c_size_t, c_ptr, c_long, c_int, c_char, c_double
implicit none

interface
    function c_strlen(string) bind(C, name='strlen')
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
    function c_basic_exp(s, a) result(res) bind(c, name='basic_exp')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_basic_log(s, a) result(res) bind(c, name='basic_log')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_basic_abs(s, a) result(res) bind(c, name='basic_abs')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_basic_sqrt(s, a) bind(c, name='basic_sqrt')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: c_basic_sqrt
    end function
    function c_basic_atan2(s, a, b) result(res) bind(c, name='basic_atan2')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_basic_evalf(s, b, bits, domain) bind(c, name='basic_evalf')
        import :: c_long, c_int, c_ptr
        type(c_ptr), value :: s
        type(c_ptr), value :: b
        integer(c_long), value :: bits
        integer(c_int), value :: domain
        integer(c_long) :: c_basic_evalf
    end function
    subroutine c_basic_const_pi(s) bind(c, name='basic_const_pi')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_e(s) bind(c, name='basic_const_E')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_euler_gamma(s) bind(c, name='basic_const_EulerGamma')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_catalan(s) bind(c, name='basic_const_Catalan')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_goldenratio(s) bind(c, name='basic_const_GoldenRatio')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_basic_max(s, d) bind(c, name='basic_max')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, d
        integer(c_long) :: c_basic_max
    end function
    function c_basic_subs2(s, e, a, b) bind(c, name='basic_subs2')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, e, a, b
        integer(c_long) :: c_basic_subs2
    end function
    function c_basic_free_symbols(self, symbols) result(res) bind(c, name='basic_free_symbols')
        import :: c_long, c_ptr
        type(c_ptr), value :: self, symbols
        integer(c_long) :: res
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
    function c_real_double_set_d(s, d) bind(c, name='real_double_set_d')
        import :: c_double, c_long, c_ptr
        type(c_ptr), value :: s
        real(c_double), value :: d
        integer(c_long) :: c_real_double_set_d
    end function
    function c_complex_set(s, re, im) result(res) bind(c, name='complex_set')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, re, im
        integer(c_long) :: res
    end function
    function c_symbol_set(s, c) bind(c, name='symbol_set')
        import c_long, c_ptr, c_char
        type(c_ptr), value :: s
        character(kind=c_char), dimension(*) :: c
        integer(c_long) :: c_symbol_set
    end function
    function c_setbasic_new() result(res) bind(c, name='setbasic_new')
        import :: c_ptr
        type(c_ptr) :: res
    end function
    subroutine c_setbasic_free(s) bind(c, name='setbasic_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_setbasic_get(s, n, r) bind(c, name='setbasic_get')
        import :: c_int, c_ptr
        type(c_ptr), value :: s, r
        integer(c_int), value :: n
    end subroutine
    function c_setbasic_insert(self, v) result(res) bind(c, name='setbasic_insert')
        import :: c_int, c_ptr
        type(c_ptr), value :: self, v
        integer(c_int) :: res
    end function
    function c_setbasic_size(s) result(res) bind(c, name='setbasic_size')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long) :: res
    end function
    function c_vecbasic_new() bind(c, name='vecbasic_new')
        import c_ptr
        type(c_ptr) :: c_vecbasic_new
    end function
    subroutine c_vecbasic_free(s) bind(c, name='vecbasic_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_vecbasic_push_back(s, val) bind(c, name='vecbasic_push_back')
        import :: c_ptr, c_long
        type(c_ptr), value :: val, s
        integer(c_long) :: c_vecbasic_push_back
    end function
    function c_dense_matrix_new() result(res) bind(c, name='dense_matrix_new')
        import :: c_ptr
        type(c_ptr) :: res
    end function
    function c_dense_matrix_new_vec(rows, cols, l) result(res) bind(c, name='dense_matrix_new_vec')
        import :: c_ptr, c_long
        integer(c_long), value :: rows, cols
        type(c_ptr), value :: l
        type(c_ptr) :: res
    end function
    subroutine c_dense_matrix_free(s) bind(c, name='dense_matrix_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    function c_dense_matrix_set(s, d) result(res) bind(c, name='dense_matrix_set')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, d
        integer(c_long) :: res
    end function
    function c_dense_matrix_add_scalar(s, a, b) result(res) bind(c, name='dense_matrix_add_scalar')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_mul_scalar(s, a, b) result(res) bind(c, name='dense_matrix_mul_scalar')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_str(s) result(res) bind(c, name='dense_matrix_str')
        import :: c_ptr
        type(c_ptr), value :: s
        type(c_ptr) :: res
    end function
    function c_dense_matrix_eq(lhs, rhs) result(res) bind(c, name='dense_matrix_eq')
        import :: c_int, c_ptr
        type(c_ptr), value :: lhs, rhs
        integer(c_int) :: res
    end function
    function c_dense_matrix_transpose(s, a) result(res) bind(c, name='dense_matrix_transpose')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
        integer(c_long) :: res
    end function
    function c_dense_matrix_mul_matrix(s, a, b) result(res) bind(c, name='dense_matrix_mul_matrix')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_add_matrix(s, a, b) result(res) bind(c, name='dense_matrix_add_matrix')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
        integer(c_long) :: res
    end function
    function c_dense_matrix_ones(s, r, c) result(res) bind(c, name='dense_matrix_ones')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: r, c
        integer(c_long) :: res
    end function
    function c_dense_matrix_zeros(s, r, c) result(res) bind(c, name='dense_matrix_zeros')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: r, c
        integer(c_long) :: res
    end function
    function c_dense_matrix_eye(s, n, m, k) result(res) bind(c, name='dense_matrix_eye')
        import :: c_long, c_int, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: n, m
        integer(c_int), value :: k
        integer(c_long) :: res
    end function

end interface 

end module
