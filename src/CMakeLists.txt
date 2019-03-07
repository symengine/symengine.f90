set(SRC
    wrapper.c
    basic.f90
    )

include_directories(BEFORE ${python_wrapper_BINARY_DIR}/symengine/lib)

add_library(symengine_f90 ${SRC})
target_link_libraries(symengine_f90 ${SYMENGINE_LIBRARIES})
