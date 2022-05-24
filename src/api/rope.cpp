#include "rope.h"

#include <cstring>
#include <libguile.h>
#include <memory>

namespace zem {

static SCM rope_type;

struct RopeBuffer {
    Rope rope;
    size_t point;

    RopeBuffer(Rope rope) : rope(rope), point(0) {}
};

static SCM zem_rope_make_rope(SCM s_init)
{
    RopeBuffer* rope;
    void* ptr;
    std::unique_ptr<char[]> init;
    init.reset(scm_to_locale_string(s_init));

    RopeBuilder b;
    b.push_leaf(init.get());

    ptr = scm_gc_malloc(sizeof(*rope), "rope");
    rope = new (ptr) RopeBuffer(b.build());

    return scm_make_foreign_object_1(rope_type, rope);
}

static SCM zem_rope_point(SCM s_rope)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);

    return scm_from_uint64(rope->point + 1);
}

static SCM zem_rope_point_min(SCM s_rope)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);

    return scm_from_uint64(1);
}

static SCM zem_rope_point_max(SCM s_rope)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);

    return scm_from_uint64(rope->rope.length() + 1);
}

static SCM zem_rope_insert_string(SCM s_rope, SCM s_string)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);
    std::unique_ptr<char[]> str;
    str.reset(scm_to_locale_string(s_string));

    rope->rope.edit(rope->point, rope->point, str.get());
    rope->point += strlen(str.get());

    return SCM_UNSPECIFIED;
}

static SCM zem_rope_insert_char(SCM s_rope, SCM s_char)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);
    char c = scm_to_char(s_char);

    rope->rope.edit(rope->point, rope->point, std::string{c});
    rope->point++;

    return SCM_UNSPECIFIED;
}

static SCM zem_rope_delete_char(SCM s_rope, SCM s_count)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);
    int64_t len = scm_to_int64(s_count);
    size_t start, end;

    if (len < 0) {
        len = -len;

        end = rope->point;
        if (len >= end)
            start = 0;
        else
            start = end - len;
    } else {
        start = rope->point;
        end = start + len;
        end = std::min(end, rope->rope.length());
    }

    rope->rope.edit(start, end, "");
    rope->point = start;

    return SCM_UNSPECIFIED;
}

static SCM zem_rope_erase(SCM s_rope)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);

    rope->rope.clear();

    return SCM_UNSPECIFIED;
}

static SCM zem_rope_goto_char(SCM s_rope, SCM s_new_point)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);
    size_t new_point = scm_to_uint64(s_new_point);

    rope->point =
        std::max(1UL, std::min(new_point, rope->rope.length() + 1)) - 1;

    return scm_from_uint64(rope->point + 1);
}

static SCM zem_rope_substr(SCM s_rope, SCM s_start, SCM s_len)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);
    size_t start = scm_to_uint64(s_start);
    size_t len = Rope::npos;

    start = std::max(1UL, std::min(start, rope->rope.length() + 1)) - 1;
    if (s_len != SCM_UNDEFINED) {
        len = scm_to_uint64(s_len);
        len = std::min(len, rope->rope.length() - start);
    }

    auto substr = rope->rope.substr(start, len);

    return scm_from_locale_stringn(substr.c_str(), substr.length());
}

static SCM zem_rope_to_string(SCM s_rope)
{
    return zem_rope_substr(s_rope, scm_from_uint64(1), SCM_UNDEFINED);
}

static void finalize_rope(SCM s_rope)
{
    RopeBuffer* rope = (RopeBuffer*)scm_foreign_object_ref(s_rope, 0);
    rope->~RopeBuffer();
}

static void zem_api_rope_init(void* data)
{
    SCM name, slots;
    scm_t_struct_finalize finalizer;

    name = scm_from_utf8_symbol("rope");
    slots = scm_list_1(scm_from_utf8_symbol("data"));

    rope_type = scm_make_foreign_object_type(name, slots, finalize_rope);

    scm_c_define_gsubr("make-rope", 1, 0, 0, (void*)zem_rope_make_rope);
    scm_c_define_gsubr("rope-point", 1, 0, 0, (void*)zem_rope_point);
    scm_c_define_gsubr("rope-point-min", 1, 0, 0, (void*)zem_rope_point_min);
    scm_c_define_gsubr("rope-point-max", 1, 0, 0, (void*)zem_rope_point_max);
    scm_c_define_gsubr("rope-insert-string!", 2, 0, 0,
                       (void*)zem_rope_insert_string);
    scm_c_define_gsubr("rope-insert-char!", 2, 0, 0,
                       (void*)zem_rope_insert_char);
    scm_c_define_gsubr("rope-delete-char!", 2, 0, 0,
                       (void*)zem_rope_delete_char);
    scm_c_define_gsubr("rope-erase!", 1, 0, 0, (void*)zem_rope_erase);
    scm_c_define_gsubr("rope-goto-char", 2, 0, 0, (void*)zem_rope_goto_char);
    scm_c_define_gsubr("rope-substr", 2, 1, 0, (void*)zem_rope_substr);
    scm_c_define_gsubr("rope->string", 1, 0, 0, (void*)zem_rope_to_string);

    scm_c_export("make-rope", "rope-point", "rope-point-min", "rope-point-max",
                 "rope-insert-string!", "rope-insert-char!",
                 "rope-delete-char!", "rope-erase!", "rope-goto-char",
                 "rope-substr", "rope->string", NULL);
}

void init_rope_api()
{
    scm_c_define_module("zem api rope", zem_api_rope_init, nullptr);
}

} // namespace zem
