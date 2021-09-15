subroutine assert_eq(a, b)
    use symengine
    type(Basic) :: a, b
    if (a /= b) then
        print *, a%str(), " /= ", b%str()
        error stop "Not equal"
    end if
end subroutine

subroutine assert_matrix_eq(a, b)
    use symengine
    type(DenseMatrix) :: a, b
    if (.not. (a == b)) then
        print *, a%str(), " /= ", b%str()
        error stop "Not equal"
    end if
end subroutine

subroutine assert_str_eq(a, b)
    use symengine
    character(len=*), intent(in) :: a, b
    if (b /= a(1:len(a)-1)) then
        print *, a, " /= ", b
        error stop "Not equal"
    end if
end subroutine



subroutine dostuff()
    use symengine
    use iso_fortran_env, only: int64
    type(Basic) :: a, b, c, d
    type(DenseMatrix) :: M, N

    a = SymInteger(12)
    b = Symbol('x')
    c = parse('x * 12')
    call assert_eq(a * b, c)
    call assert_eq(b * 12, c)
    call assert_eq(12 * b, c)
    call assert_eq(b * 12_int64, c)
    call assert_eq(12_int64 * b, c)

    c = parse('x + 12')
    call assert_eq(a + b, c)
    call assert_eq(b + 12, c)
    call assert_eq(12 + b, c)
    call assert_eq(b + 12_int64, c)
    call assert_eq(12_int64 + b, c)

    c = parse('12 - x')
    call assert_eq(a - b, c)
    call assert_eq(12 - b, c)
    call assert_eq(12_int64 - b, c)
    c = parse('x - 12')
    call assert_eq(b - 12, c)
    call assert_eq(b - 12_int64, c)

    c = parse('-x')
    call assert_eq(-b, c)

    c = parse('12 / x')
    call assert_eq(a / b, c)
    call assert_eq(12 / b, c)
    call assert_eq(12_int64 / b, c)
    c = parse('x / 12')
    call assert_eq(b / 12, c)
    call assert_eq(b / 12_int64, c)

    c = parse('12 ** x')
    call assert_eq(a ** b, c)
    call assert_eq(12 ** b, c)
    call assert_eq(12_int64 ** b, c)
    c = parse('x ** 12')
    call assert_eq(b ** 12, c)
    call assert_eq(b ** 12_int64, c)

    c = parse('sin(x)')
    call assert_eq(sin(b), c)

    c = parse('cos(x)')
    call assert_eq(cos(b), c)

    c = parse('sqrt(x)')
    call assert_eq(sqrt(b), c)

    a = Rational(1, 2)
    b = Rational(3, 4)
    c = Rational(3, 8)
    call assert_eq(a * b, c)

    a = RealDouble(1.0)
    b = RealDouble(2.0)
    c = RealDouble(3.0)
    call assert_eq(a + b, c)

    a = Symbol('x')
    b = parse('x * 2.0')
    call assert_eq(a * 2.0, b)
    call assert_eq(2.0 * a, b)
    call assert_eq(a * 2.0d0, b)
    call assert_eq(2.0d0 * a, b)

    a = Symbol('x')
    b = parse('x + 1.0')
    call assert_eq(a + 1.0, b)
    call assert_eq(1.0 + a, b)
    call assert_eq(a + 1.0d0, b)
    call assert_eq(1.0d0 + a, b)

    a = Symbol('x')
    b = parse('x - 1.0')
    call assert_eq(a - 1.0, b)
    call assert_eq(a - 1.0d0, b)
    b = parse('1.0 - x')
    call assert_eq(1.0 - a, b)
    call assert_eq(1.0d0 - a, b)

    a = Symbol('x')
    b = parse('x / 2.0')
    call assert_eq(a / 2.0, b)
    call assert_eq(a / 2.0d0, b)
    b = parse('2.0 / x')
    call assert_eq(2.0 / a, b)
    call assert_eq(2.0d0 / a, b)

    a = Symbol('x')
    b = parse('x ** 2.0')
    call assert_eq(a ** 2.0, b)
    call assert_eq(a ** 2.0d0, b)
    b = parse('2.0 ** x')
    call assert_eq(2.0 ** a, b)
    call assert_eq(2.0d0 ** a, b)

    a = RealDouble(1.0d0)
    b = RealDouble(2.0d0)
    c = RealDouble(3.0d0)
    call assert_eq(a + b, c)

    a = SymInteger(2)
    c = sqrt(a)
    c = c%evalf(53_8, 1)
    d = RealDouble(1.4142135623730951d0)
    call assert_eq(c, d)

    a = pi()
    b = parse("pi")
    call assert_eq(a, b)

    a = e()
    b = parse("e")
    call assert_eq(a, b)

    a = eulergamma()
    b = parse("EulerGamma")
    call assert_eq(a, b)

    a = catalan()
    b = parse("Catalan")
    call assert_eq(a, b)

    a = goldenratio()
    b = parse("GoldenRatio")
    call assert_eq(a, b)

    a = max([ptr(SymInteger(2)), ptr(SymInteger(3))])
    b = parse("3")
    call assert_eq(a, b)

    a = max([ptr(Symbol("x")), ptr(SymInteger(3))])
    b = parse("max(3, x)")
    call assert_eq(a, b)

    a = Symbol("x")
    b = SymInteger(2) * a
    c = b%subs(a, SymInteger(15))
    d = SymInteger(30)
    call assert_eq(c, d)

    a = SymComplex(1, 1)
    b = parse('1 + I')
    call assert_eq(a, b)

    a = SymComplex(1_int64, 1_int64)
    b = parse('1 + I')
    call assert_eq(a, b)
    a = SymComplex(1, 1_int64)
    call assert_eq(a, b)
    a = SymComplex(1_int64, 1)
    call assert_eq(a, b)

    a = SymInteger(23)
    b = SymInteger(45)
    c = SymComplex(a, b)
    d = parse('23 + 45*I')
    call assert_eq(c, d)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
    M = M + SymInteger(2)
    call assert_str_eq(M%str(), '[3, 4]')
    call assert_matrix_eq(M, N)
    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    M = SymInteger(2) + M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
    M = M + 2
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
    M = 2 + M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
    M = M + 2_int64
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
    M = 2_int64 + M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
    M = M + 2.0
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
    M = 2.0 + M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
    M = M + 2.0d0
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
    M = 2.0d0 + M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(-2))])
    M = -M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(0))])
    M = M - SymInteger(2)
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(0))])
    M = SymInteger(2) - M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(0))])
    M = M - 2
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(0))])
    M = 2 - M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(0))])
    M = M - 2_int64
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(0))])
    M = 2_int64 - M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(3))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(-1.0)), ptr(RealDouble(1.0))])
    M = M - 2.0
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(3))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(1.0)), ptr(RealDouble(-1.0))])
    M = 2.0 - M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    M = M * SymInteger(2)
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    M = SymInteger(2) * M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    M = M * 2
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    M = 2 * M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    M = M * 2_int64
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    M = 2_int64 * M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
    M = M * 2.0
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
    M = 2.0 * M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
    M = M * 2.0d0
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
    M = 2.0d0 * M
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    M = M / SymInteger(2)
    call assert_matrix_eq(M, N)
   
    M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    M = M / 2
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    M = M / 2_int64
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(1.0)), ptr(RealDouble(2.0))])
    M = M / 2.0
    call assert_matrix_eq(M, N)

    M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
    N = DenseMatrix(1, 2, [ptr(RealDouble(1.0)), ptr(RealDouble(2.0))])
    M = M / 2.0d0
    call assert_matrix_eq(M, N)

    M = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(2)), ptr(SymInteger(3)), ptr(SymInteger(4))])
    N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(3)), ptr(SymInteger(2)), ptr(SymInteger(4))])
    M = transpose(M)
    call assert_matrix_eq(M, N)

    M = DenseMatrix(2, 1, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    M = M * N
    N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(2)), ptr(SymInteger(2)), ptr(SymInteger(4))])
    call assert_matrix_eq(M, N)

    M = DenseMatrix(2, 1, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(2, 1, [ptr(SymInteger(3)), ptr(SymInteger(4))])
    M = M + N
    N = DenseMatrix(2, 1, [ptr(SymInteger(4)), ptr(SymInteger(6))])
    call assert_matrix_eq(M, N)

    M = DenseMatrix(2, 1, [ptr(SymInteger(1)), ptr(SymInteger(2))])
    N = DenseMatrix(2, 1, [ptr(SymInteger(3)), ptr(SymInteger(4))])
    M = M - N
    N = DenseMatrix(2, 1, [ptr(SymInteger(-2)), ptr(SymInteger(-2))])
    call assert_matrix_eq(M, N)

    M = ones(2, 2)
    N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(1)), ptr(SymInteger(1)), ptr(SymInteger(1))])
    call assert_matrix_eq(M, N)

    M = zeros(2, 2)
    N = DenseMatrix(2, 2, [ptr(SymInteger(0)), ptr(SymInteger(0)), ptr(SymInteger(0)), ptr(SymInteger(0))])
    call assert_matrix_eq(M, N)

    M = eye(2, 0)
    N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(0)), ptr(SymInteger(0)), ptr(SymInteger(1))])
    call assert_matrix_eq(M, N)

end subroutine



program test

    implicit none

    call dostuff

    print *, "Finishing"

end program
