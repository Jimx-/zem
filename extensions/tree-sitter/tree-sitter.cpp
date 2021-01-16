#include <cassert>
#include <cstring>
#include <libguile.h>
#include <memory>
#include <tree_sitter/api.h>
#include <vector>

static SCM parser_type;
static SCM tree_type;
static SCM node_type;
static SCM query_cursor_type;
static SCM query_type;

struct SCMTree {
    TSTree* tree;
    bool dirty;
};

struct SCMQuery {
    TSQuery* query;
    std::vector<SCM> tags;
};

static TSPoint extract_tspoint(SCM val)
{
    TSPoint pt;

    pt.row = scm_to_int(scm_car(val));
    pt.column = scm_to_int(scm_cdr(val));

    return pt;
}

static SCM zem_ts_parser_new(SCM s_lang)
{
    TSParser* parser;
    TSLanguage* lang = (TSLanguage*)scm_foreign_object_ref(s_lang, 0);

    parser = ts_parser_new();

    ts_parser_set_language(parser, lang);

    return scm_make_foreign_object_1(parser_type, parser);
}

static SCM zem_ts_parser_parse_string(SCM s_parser, SCM s_str, SCM s_old_tree)
{
    TSParser* parser = (TSParser*)scm_foreign_object_ref(s_parser, 0);
    SCMTree* old_tree = nullptr;
    SCMTree* new_tree;
    std::unique_ptr<char[]> str;
    str.reset(scm_to_locale_string(s_str));

    new_tree = (SCMTree*)scm_gc_malloc(sizeof(*new_tree), "tree-sitter");

    if (s_old_tree != SCM_UNDEFINED)
        old_tree = (SCMTree*)scm_foreign_object_ref(s_old_tree, 0);

    if (!old_tree || old_tree->dirty) {
        new_tree->tree =
            ts_parser_parse_string(parser, old_tree ? old_tree->tree : nullptr,
                                   str.get(), strlen(str.get()));
    } else {
        new_tree->tree = ts_tree_copy(old_tree->tree);
    }

    new_tree->dirty = false;

    return scm_make_foreign_object_1(tree_type, new_tree);
}

static SCM zem_ts_tree_root_node(SCM s_tree)
{
    SCMTree* tree = (SCMTree*)scm_foreign_object_ref(s_tree, 0);
    TSNode* root;
    root = (TSNode*)scm_gc_malloc(sizeof(*root), "tree-sitter");

    *root = ts_tree_root_node(tree->tree);

    return scm_make_foreign_object_1(node_type, root);
}

static SCM zem_ts_tree_edit(SCM s_tree, SCM s_start_byte, SCM s_old_end_byte,
                            SCM s_new_end_byte, SCM s_start_point,
                            SCM s_old_end_point, SCM s_new_end_point)
{
    TSInputEdit edit;
    SCMTree* tree = (SCMTree*)scm_foreign_object_ref(s_tree, 0);

    edit.start_byte = scm_to_uint32(s_start_byte) - 1;
    edit.old_end_byte = scm_to_uint32(s_old_end_byte) - 1;
    edit.new_end_byte = scm_to_uint32(s_new_end_byte) - 1;
    edit.start_point = extract_tspoint(s_start_point);
    edit.old_end_point = extract_tspoint(s_old_end_point);
    edit.new_end_point = extract_tspoint(s_new_end_point);

    ts_tree_edit(tree->tree, &edit);
    tree->dirty = true;

    return SCM_UNSPECIFIED;
}

static SCM zem_ts_tree_changed_ranges(SCM s_old_tree, SCM s_new_tree)
{
    SCMTree* old_tree = (SCMTree*)scm_foreign_object_ref(s_old_tree, 0);
    SCMTree* new_tree = (SCMTree*)scm_foreign_object_ref(s_new_tree, 0);
    uint32_t length;
    TSRange* ranges =
        ts_tree_get_changed_ranges(old_tree->tree, new_tree->tree, &length);

    SCM result = scm_list_n(SCM_UNDEFINED);
    for (int i = length - 1; i >= 0; i--) {
        TSRange* range = &ranges[i];
        SCM s_range = scm_cons(scm_from_uint32(range->start_byte),
                               scm_from_uint32(range->end_byte));

        result = scm_cons(s_range, result);
    }

    return result;
}

static SCM zem_ts_query_cursor_new()
{
    TSQueryCursor* cursor;

    cursor = ts_query_cursor_new();

    return scm_make_foreign_object_1(query_cursor_type, cursor);
}

static SCM zem_ts_query_cursor_set_byte_range(SCM s_cursor, SCM s_beg,
                                              SCM s_end)
{
    TSQueryCursor* cursor = (TSQueryCursor*)scm_foreign_object_ref(s_cursor, 0);
    uint32_t beg = scm_to_uint32(s_beg);
    uint32_t end = scm_to_uint32(s_end);

    ts_query_cursor_set_byte_range(cursor, beg, end);

    return SCM_UNSPECIFIED;
}

static SCM zem_ts_query_cursor_captures(SCM s_cursor, SCM s_query, SCM s_node)
{
    struct Capture {
        uint32_t start;
        uint32_t end;
        SCM tag;
        Capture(uint32_t start, uint32_t end, SCM tag)
            : start(start), end(end), tag(tag)
        {}
    };

    TSQueryCursor* cursor = (TSQueryCursor*)scm_foreign_object_ref(s_cursor, 0);
    SCMQuery* query = (SCMQuery*)scm_foreign_object_ref(s_query, 0);
    TSNode* node = (TSNode*)scm_foreign_object_ref(s_node, 0);
    TSQueryMatch match;
    uint32_t index = 0;
    std::vector<Capture> captures;

    ts_query_cursor_exec(cursor, query->query, *node);

    while (ts_query_cursor_next_capture(cursor, &match, &index)) {
        const TSQueryCapture* capture = &match.captures[index];
        uint32_t start = ts_node_start_byte(capture->node);
        uint32_t end = ts_node_end_byte(capture->node);
        uint32_t len;

        captures.emplace_back(start, end, query->tags[capture->index]);
    }

    SCM result = scm_list_n(SCM_UNDEFINED);
    for (int i = captures.size() - 1; i >= 0; i--) {
        SCM pos = scm_cons(scm_from_uint32(captures[i].start),
                           scm_from_uint32(captures[i].end));
        result = scm_cons(scm_cons(pos, captures[i].tag), result);
    }

    return result;
}

static SCM zem_ts_query_new(SCM s_lang, SCM s_source, SCM s_mapper)
{
    TSLanguage* lang = (TSLanguage*)scm_foreign_object_ref(s_lang, 0);
    void* ptr;
    SCMQuery* query;
    uint32_t error_offset;
    TSQueryError error_type;
    std::unique_ptr<char[]> source;
    source.reset(scm_to_locale_string(s_source));

    ptr = scm_gc_malloc(sizeof(SCMQuery), "tree-sitter");
    query = new (ptr) SCMQuery;

    query->query = ts_query_new(lang, source.get(), strlen(source.get()),
                                &error_offset, &error_type);
    assert(error_type == TSQueryErrorNone);

    for (int i = 0; i < ts_query_capture_count(query->query); i++) {
        uint32_t len;
        const char* name = ts_query_capture_name_for_id(query->query, i, &len);
        SCM s_name = scm_from_utf8_string(name);

        query->tags.push_back(scm_call_1(s_mapper, s_name));
    }

    return scm_make_foreign_object_1(query_type, query);
}

static void finalize_parser(SCM s_parser)
{
    TSParser* parser = (TSParser*)scm_foreign_object_ref(s_parser, 0);
    ts_parser_delete(parser);
}

static void finalize_tree(SCM s_tree)
{
    SCMTree* tree = (SCMTree*)scm_foreign_object_ref(s_tree, 0);
    ts_tree_delete(tree->tree);
}

static void finalize_node(SCM s_node) {}

static void finalize_query_cursor(SCM s_cursor)
{
    TSQueryCursor* cursor = (TSQueryCursor*)scm_foreign_object_ref(s_cursor, 0);
    ts_query_cursor_delete(cursor);
}

static void finalize_query(SCM s_query)
{
    SCMQuery* query = (SCMQuery*)scm_foreign_object_ref(s_query, 0);
    ts_query_delete(query->query);
}

extern "C"
{
    void init_tree_sitter()
    {
        SCM name, slots;
        scm_t_struct_finalize finalizer;

        name = scm_from_utf8_symbol("tree-sitter-parser");
        slots = scm_list_1(scm_from_utf8_symbol("data"));
        parser_type =
            scm_make_foreign_object_type(name, slots, finalize_parser);

        name = scm_from_utf8_symbol("tree-sitter-tree");
        slots = scm_list_1(scm_from_utf8_symbol("data"));
        tree_type = scm_make_foreign_object_type(name, slots, finalize_tree);

        name = scm_from_utf8_symbol("tree-sitter-node");
        slots = scm_list_1(scm_from_utf8_symbol("data"));
        node_type = scm_make_foreign_object_type(name, slots, finalize_node);

        name = scm_from_utf8_symbol("tree-sitter-query-cusor");
        slots = scm_list_1(scm_from_utf8_symbol("data"));
        query_cursor_type =
            scm_make_foreign_object_type(name, slots, finalize_query_cursor);

        name = scm_from_utf8_symbol("tree-sitter-query");
        slots = scm_list_1(scm_from_utf8_symbol("data"));
        query_type = scm_make_foreign_object_type(name, slots, finalize_query);

        scm_c_define_gsubr("parser-new", 1, 0, 0, (void*)zem_ts_parser_new);
        scm_c_define_gsubr("parser-parse-string", 2, 1, 0,
                           (void*)zem_ts_parser_parse_string);
        scm_c_define_gsubr("tree-root-node", 1, 0, 0,
                           (void*)zem_ts_tree_root_node);
        scm_c_define_gsubr("tree-edit", 7, 0, 0, (void*)zem_ts_tree_edit);
        scm_c_define_gsubr("tree-changed-ranges", 2, 0, 0,
                           (void*)zem_ts_tree_changed_ranges);
        scm_c_define_gsubr("query-cursor-new", 0, 0, 0,
                           (void*)zem_ts_query_cursor_new);
        scm_c_define_gsubr("query-cursor-set-byte-range", 3, 0, 0,
                           (void*)zem_ts_query_cursor_set_byte_range);
        scm_c_define_gsubr("query-cursor-captures", 3, 0, 0,
                           (void*)zem_ts_query_cursor_captures);
        scm_c_define_gsubr("query-new", 3, 0, 0, (void*)zem_ts_query_new);

        scm_c_export("parser-new", "parser-parse-string", "tree-root-node",
                     "tree-edit", "tree-changed-ranges", "query-cursor-new",
                     "query-cursor-set-byte-range", "query-cursor-captures",
                     "query-new", NULL);
    }
}
