find_package(tree-sitter REQUIRED)
find_package(tree-sitter-c REQUIRED)

set(EXT_SRCLIST tree-sitter-cpp/src/parser.c
                tree-sitter-cpp/src/scanner.c)
    
add_library(guile-tree-sitter-cpp MODULE tree-sitter-cpp.c ${EXT_SRCLIST})

target_link_libraries(guile-tree-sitter-cpp
    tree-sitter::tree-sitter
    tree-sitter-c::tree-sitter-c)
