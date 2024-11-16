#include <libguile.h>
#include <tree_sitter/api.h>

const TSLanguage* tree_sitter_c(void);
const TSLanguage* tree_sitter_cpp(void);

static SCM language_type;

static SCM zem_ts_language_c()
{
    return scm_make_foreign_object_1(language_type, (void*)tree_sitter_c());
}

static SCM zem_ts_language_cpp()
{
    return scm_make_foreign_object_1(language_type, (void*)tree_sitter_cpp());
}

void init_tree_sitter_cc()
{
    SCM name, slots;
    scm_t_struct_finalize finalizer;

    name = scm_from_utf8_symbol("tree-sitter-language");
    slots = scm_list_1(scm_from_utf8_symbol("data"));
    language_type = scm_make_foreign_object_type(name, slots, NULL);

    scm_c_define_gsubr("tree-sitter-language-c", 0, 0, 0,
                       (void*)zem_ts_language_c);
    scm_c_define_gsubr("tree-sitter-language-cpp", 0, 0, 0,
                       (void*)zem_ts_language_cpp);
}
