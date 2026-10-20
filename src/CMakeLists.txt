set(SRC
    conversion.f90
    exceptions.f90
    symengine_interface.f90
    symengine_basic.f90
    symengine_rational.f90
    symengine_symbol.f90
    functions.f90
    constants.f90
    dense_matrix.f90
    sets.f90
    lambdify.F90
    symengine.f90
    )
add_library(symengine_f90 ${SRC})
target_link_libraries(symengine_f90 ${SYMENGINE_LIBRARIES})
if (HAVE_SYMENGINE_LLVM)
    target_compile_definitions(symengine_f90 PRIVATE HAVE_SYMENGINE_LLVM)
endif()

install(TARGETS symengine_f90 DESTINATION lib)
