configure_file(lit.site.cfg.py.in ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py)

macro(xtest_add_test TESTNAME)
    add_test(NAME test.${TESTNAME}
            COMMAND ./my-lit.py -j 1 -v ${TESTNAME}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro() 

xtest_add_test(sample)
xtest_add_test(reports)
xtest_add_test(help)
