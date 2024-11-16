find_package(tree-sitter REQUIRED)

add_library(guile-tree-sitter MODULE tree-sitter.cpp ${EXT_SRCLIST})

target_link_libraries(guile-tree-sitter tree-sitter::tree-sitter)
