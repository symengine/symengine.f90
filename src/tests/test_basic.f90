subroutine assert_eq(a, b)
    use symengine
    type(Basic) :: a, b
    if (a /= b) then
        stop 1
    end if
end subroutine


subroutine dostuff()
    use symengine
    type(Basic) :: a, b, c

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

    a = Rational(1, 2)
    b = Rational(3, 4)
    c = Rational(3, 8)
    call assert_eq(a * b, c)
end subroutine



program test

    implicit none

    call dostuff

    print *, "Finishing"

end program
