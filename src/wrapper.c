#include <symengine/cwrapper.h>

void f(int i)
{
    char *s;
    basic x, y, e, n;
    basic_new_stack(x);
    basic_new_stack(y);
    basic_new_stack(e);
    basic_new_stack(n);
    symbol_set(x, "x");
    symbol_set(y, "y");
    integer_set_si(n, i);
    basic_mul(e, n, x);
    basic_add(e, e, y);
    s = basic_str(e);
    printf("Result: %s", s);
    basic_str_free(s);
}
