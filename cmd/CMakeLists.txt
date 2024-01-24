#
# CMakeLists.txt build executables
#

include_directories(../src)

add_executable(rim rim.f90)
add_executable(rime rime.f)
add_executable(rimh rimh.f)
add_executable(lxtest lxtest.f90)
add_executable(rmtest rmtest.f)

target_link_libraries(rim
     PRIVATE
     rimintlib)

target_link_libraries(rime
     PRIVATE
     rimintlib)

target_link_libraries(rimh
     PRIVATE
     rimintlib rimlib)

target_link_libraries(lxtest
    PRIVATE
    rimintlib system)

target_link_libraries(rmtest
    PRIVATE
    rimlib system)

install(TARGETS rim DESTINATION bin)
install(TARGETS rimh DESTINATION bin)
