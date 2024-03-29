cmake_minimum_required(VERSION 3.11)

project(zem)

set(CMAKE_CXX_STANDARD 17)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

set(TOPDIR ${PROJECT_SOURCE_DIR})

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

find_package(Guile REQUIRED)

find_package(Readline REQUIRED)

find_package(Freetype REQUIRED)

include_directories(${TOPDIR}/include/
    ${TOPDIR}/modules/emacsy/
    ${CONAN_INCLUDE_DIRS}
    ${GUILE_INCLUDE_DIR}
    ${FREETYPE_INCLUDE_DIR}
    )

add_subdirectory(extensions)

set(ZEM_SRCLIST
    src/main.cpp
    src/intern_string.cpp
    src/shader_program.cpp
    src/renderer.cpp
    src/font.cpp
    src/keymap.c
    src/rope.cpp
    src/api/api.cpp
    src/api/renderer.cpp
    src/api/font.cpp
    src/api/rope.cpp
    )

set(EXT_SRCLIST
    modules/emacsy/emacsy.c
    )

add_executable(zem ${ZEM_SRCLIST} ${EXT_SRCLIST})
    
target_link_libraries(zem CONAN_PKG::glad
                          CONAN_PKG::glfw
                          CONAN_PKG::spdlog
                          ${GUILE_LIBRARY}
                          ${READLINE_LIBRARY}
                          ${FREETYPE_LIBRARIES})
