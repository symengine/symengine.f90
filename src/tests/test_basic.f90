subroutine assert_eq(a, b)
    use symengine
    type(Basic) :: a, b
    if (a /= b) then
        error stop "Not equal"
    end if
end subroutine


subroutine dostuff()
    use symengine
    type(Basic) :: a, b, c, d

    a = SymInteger(12)
    b = Symbol('x')
    c = parse('x * 12')
    call assert_eq(a * b, c)

    c = parse('x + 12')
    call assert_eq(a + b, c)

    c = parse('12 - x')
    call assert_eq(a - b, c)

    c = parse('12 / x')
    call assert_eq(a / b, c)

    c = parse('12 ** x')
    call assert_eq(a ** b, c)

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

    a = RealDouble(1.0d0)
    b = RealDouble(2.0d0)
    c = RealDouble(3.0d0)
    call assert_eq(a + b, c)

    a = SymInteger(2)
    c = sqrt(a)
    c = c%evalf(53_8, 1)
    d = RealDouble(1.4142135623730951d0)
    call assert_eq(c, d)
end subroutine



program test

    implicit none

    call dostuff

    print *, "Finishing"

end program
