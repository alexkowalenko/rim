#
# CMakeLists.txt build executables
#

include_directories(../src)

add_executable(rim rim.f90)
add_executable(rime rime.f90)
add_executable(rimh rimh.f90)
add_executable(lxtest lxtest.f90)
add_executable(rmtest rmtest.f90)
add_executable(test1 test1.f90)

target_link_libraries(rim
     PRIVATE
     rimcommon rimextlib)

target_link_libraries(rime
     PRIVATE
     rimcommon rimextlib)

target_link_libraries(rimh
     PRIVATE
     rimcommon rimlib)

target_link_libraries(lxtest
    PRIVATE
    rimcommon rimextlib)

target_link_libraries(rmtest
    PRIVATE
    rimcommon rimlib)

install(TARGETS rim DESTINATION bin)
install(TARGETS rimh DESTINATION bin)
install(TARGETS lxtest DESTINATION bin)
