subroutine dostuff()
    use symengine
    type(Basic) :: a, b, c

    a = SymInteger(12)
    b = Symbol('x')
    c = a * b
    print *, c%str()
    c = parse('2*(24+x)')
    print *, c%str()
end subroutine



program test

    implicit none

    call dostuff

    print *, "Finishing"

end program
