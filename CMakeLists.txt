cmake_minimum_required(VERSION 3.20)

set(AX_LLVM /opt/homebrew/Cellar/llvm/18.0.0)
#set(CMAKE_Fortran_COMPILER ${AX_LLVM}/bin/flang-new -L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib)

set(CMAKE_BUILD_TYPE Debug)

project(rim VERSION 0.1
        DESCRIPTION "RIM"
        LANGUAGES Fortran)
enable_language(Fortran)

### Require out-of-source builds
file(TO_CMAKE_PATH "${PROJECT_BINARY_DIR}/CMakeLists.txt" LOC_PATH)
if(EXISTS "${LOC_PATH}")
    message(FATAL_ERROR "You cannot build in a source directory (or any directory with a CMakeLists.txt file). Please make a build subdirectory. Feel free to remove CMakeCache.txt and CMakeFiles.")
endif()

set(FVERSION "-std=legacy") # -std=f95, f2003, f2008, gnu, legacy
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FVERSION}")

add_subdirectory(src)