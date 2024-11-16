cmake_minimum_required(VERSION 3.11)

project(zem)

set(CMAKE_CXX_STANDARD 17)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

set(TOPDIR ${PROJECT_SOURCE_DIR})

find_package(Guile REQUIRED)
find_package(Readline REQUIRED)
find_package(Freetype REQUIRED)

find_package(glad)
find_package(glfw3)
find_package(glm)
find_package(spdlog)

include_directories(${TOPDIR}/include/
    ${TOPDIR}/modules/emacsy/
    ${GUILE_INCLUDE_DIR}
    ${FREETYPE_INCLUDE_DIRS}
    )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)

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
    
target_link_libraries(zem glad::glad
                          glfw
                          glm::glm
                          spdlog::spdlog
                          ${GUILE_LIBRARY}
                          ${READLINE_LIBRARY}
                          ${FREETYPE_LIBRARIES})

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
    
