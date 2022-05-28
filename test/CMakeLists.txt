include_directories(${PROJECT_BINARY_DIR}/src)

macro(ADDTEST name)
    add_executable(${name} ${name}.f90)
    target_link_libraries(${name} symengine_f90)
    add_test(${name} ${PROJECT_BINARY_DIR}/${name})
endmacro(ADDTEST)

project(tests)

ADDTEST(test_basic)
