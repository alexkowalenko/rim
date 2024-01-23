configure_file(lit.site.cfg.py.in ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py)

macro(xtest_add_test TESTNAME)
    add_test(NAME xtest.${TESTNAME}
            COMMAND ./my-lit.py -v ${TESTNAME}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro() 

xtest_add_test(sample)
