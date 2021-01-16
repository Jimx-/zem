set(EXT_SRCLIST tree-sitter-cpp/src/parser.c
                tree-sitter-cpp/src/scanner.cc)
    
add_library(guile-tree-sitter-cpp MODULE tree-sitter-cpp.c ${EXT_SRCLIST})

target_link_libraries(guile-tree-sitter-cpp CONAN_PKG::tree-sitter)
