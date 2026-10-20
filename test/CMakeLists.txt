include_directories(${PROJECT_BINARY_DIR}/src)

macro(ADDTEST name)
    add_executable(${name} ${name}.f90)
    target_link_libraries(${name} symengine_f90)
    add_test(${name} ${PROJECT_BINARY_DIR}/${name})
endmacro(ADDTEST)

project(tests)

ADDTEST(test_basic)

add_executable(test_lambdify test_lambdify.F90)
target_link_libraries(test_lambdify symengine_f90)
if (HAVE_SYMENGINE_LLVM)
    target_compile_definitions(test_lambdify PRIVATE HAVE_SYMENGINE_LLVM)
endif()
add_test(test_lambdify ${PROJECT_BINARY_DIR}/test_lambdify)
