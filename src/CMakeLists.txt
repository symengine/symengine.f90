set(SRC
    symengine.f90
    )
add_library(symengine_f90 ${SRC})
target_link_libraries(symengine_f90 ${SYMENGINE_LIBRARIES})

add_subdirectory(tests)
