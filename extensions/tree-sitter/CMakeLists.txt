set(EXT_SRCLIST ${TOPDIR}/3rdparty/tree-sitter-cpp/src/parser.c
                ${TOPDIR}/3rdparty/tree-sitter-cpp/src/scanner.cc)
    
add_library(guile-tree-sitter MODULE tree-sitter.cpp ${EXT_SRCLIST})

target_link_libraries(guile-tree-sitter CONAN_PKG::tree-sitter)
