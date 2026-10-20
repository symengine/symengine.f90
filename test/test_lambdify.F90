program test_lambdify
    use symengine
    use iso_c_binding, only: c_double

    implicit none

    call test_point()
    call test_batch()
    call test_basic_array()
#ifdef HAVE_SYMENGINE_LLVM
    call test_llvm()
#endif

    print *, "Finishing"

    contains

    subroutine assert_close(a, b)
        real(c_double), intent(in) :: a, b
        if (abs(a - b) > 1e-12_c_double) then
            print *, a, " /= ", b
            error stop "Not equal"
        end if
    end subroutine

    subroutine assert(a)
        logical :: a
        if (.not. a) then
            error stop "Assertion failed"
        end if
    end subroutine

    subroutine test_point()
        type(Basic) :: x, y
        type(Lambdify) :: f
        real(c_double) :: outs(1)
        real(c_double), parameter :: vals(3) = [-1.0_c_double, 0.0_c_double, 1.0_c_double]
        integer :: c, i, j

        x = Symbol('x')
        y = Symbol('y')
        do c = 0, 1
            call f%init([ptr(x), ptr(y)], [ptr(parse('x^2 + y^2'))], cse=(c == 1))
            call assert(f%num_inputs() == 2)
            call assert(f%num_outputs() == 1)
            do i = 1, 3
                do j = 1, 3
                    call f%eval([vals(i), vals(j)], outs)
                    call assert_close(outs(1), vals(i)**2 + vals(j)**2)
                end do
            end do
        end do
        print *, "test_point: success"
    end subroutine

    subroutine test_batch()
        type(Basic) :: x, y, z
        type(Lambdify) :: f
        real(c_double) :: inps(3, 2), outs(2, 2)

        x = Symbol('x')
        y = Symbol('y')
        z = Symbol('z')
        ! r = x + y*z + (y*z)**2, s = 2*x + y*z + (y*z)**2
        call f%init([ptr(x), ptr(y), ptr(z)], &
                    [ptr(x + y*z + (y*z)**2), ptr(2*x + y*z + (y*z)**2)], cse=.true.)
        inps(:, 1) = [1.5_c_double, 2.0_c_double, 3.0_c_double]
        inps(:, 2) = [1.0_c_double, 1.0_c_double, 1.0_c_double]
        call f%eval(inps, outs)
        call assert_close(outs(1, 1), 43.5_c_double)
        call assert_close(outs(2, 1), 45.0_c_double)
        call assert_close(outs(1, 2), 3.0_c_double)
        call assert_close(outs(2, 2), 4.0_c_double)
        print *, "test_batch: success"
    end subroutine

    subroutine test_basic_array()
        type(Basic) :: args(2), exprs(2)
        type(Lambdify) :: f
        real(c_double) :: outs(2)

        args(1) = Symbol('x')
        args(2) = Symbol('y')
        exprs(1) = sin(args(1)) * args(2)
        exprs(2) = args(1) + args(2)**2
        call f%init(args, exprs)
        call f%eval([1.0_c_double, 2.0_c_double], outs)
        call assert_close(outs(1), sin(1.0_c_double) * 2.0_c_double)
        call assert_close(outs(2), 5.0_c_double)

        ! Re-initializing frees the previous function
        call f%init(args, exprs(2:2))
        call assert(f%num_outputs() == 1)
        call f%eval([1.0_c_double, 2.0_c_double], outs(1:1))
        call assert_close(outs(1), 5.0_c_double)
        print *, "test_basic_array: success"
    end subroutine

#ifdef HAVE_SYMENGINE_LLVM
    subroutine test_llvm()
        type(Basic) :: x, y
        type(Lambdify) :: f
        procedure(lambdify_function), pointer :: fn
        real(c_double) :: inps(2, 9), outs(1, 9), out(1)
        real(c_double), parameter :: vals(3) = [-1.0_c_double, 0.0_c_double, 1.0_c_double]
        integer :: i, j

        x = Symbol('x')
        y = Symbol('y')
        call f%init([ptr(x), ptr(y)], [ptr(x**2 + y**2)], backend='llvm')
        do i = 1, 3
            do j = 1, 3
                inps(:, 3 * (i - 1) + j) = [vals(i), vals(j)]
            end do
        end do
        call f%eval(inps, outs)
        do j = 1, 9
            call assert_close(outs(1, j), inps(1, j)**2 + inps(2, j)**2)
        end do

        ! The compiled function itself
        call f%get_function(fn)
        do j = 1, 9
            call fn(inps(:, j), out)
            call assert_close(out(1), inps(1, j)**2 + inps(2, j)**2)
        end do
        print *, "test_llvm: success"
    end subroutine
#endif

end program
