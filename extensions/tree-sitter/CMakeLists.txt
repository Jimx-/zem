add_library(guile-tree-sitter MODULE tree-sitter.cpp ${EXT_SRCLIST})

target_link_libraries(guile-tree-sitter CONAN_PKG::tree-sitter)
