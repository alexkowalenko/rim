#
# CMakeLists.txt build executables
#

include_directories(../src)

add_executable(rim rim.f)
add_executable(rime rime.f)
add_executable(rimh rimh.f)
add_executable(lxtest lxtest.f)
add_executable(rmtest rmtest.f)

target_link_libraries(rim
     PRIVATE
     rimintlib)

target_link_libraries(rime
     PRIVATE
     rimintlib)

target_link_libraries(rimh
     PRIVATE
     rimlib)

target_link_libraries(lxtest
    PRIVATE
    rimintlib)

target_link_libraries(rmtest
    PRIVATE
    rimlib)

