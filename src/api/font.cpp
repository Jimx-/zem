#include "font.h"

#include <libguile.h>
#include <memory>

namespace zem {

static SCM font_type;

static SCM zem_font_load_font(SCM s_name, SCM s_size)
{
    Font* font;
    void* ptr;
    std::unique_ptr<char[]> name;
    name.reset(scm_to_locale_string(s_name));

    ptr = scm_gc_malloc(sizeof(*font), "font");
    font = new (ptr) Font(std::string(name.get()), scm_to_uint(s_size));

    return scm_make_foreign_object_1(font_type, font);
}

static SCM zem_font_get_font_height(SCM s_font)
{
    Font* font = (Font*)scm_foreign_object_ref(s_font, 0);

    return scm_from_uint(font->get_height_pixels());
}

static void finalize_font(SCM s_font)
{
    Font* font = (Font*)scm_foreign_object_ref(s_font, 0);
    font->~Font();
}

static void zem_api_font_init(void* data)
{
    SCM name, slots;
    scm_t_struct_finalize finalizer;

    name = scm_from_utf8_symbol("font");
    slots = scm_list_1(scm_from_utf8_symbol("data"));

    font_type = scm_make_foreign_object_type(name, slots, finalize_font);

    scm_c_define_gsubr("load-font", 2, 0, 0, (void*)zem_font_load_font);
    scm_c_define_gsubr("get-font-height", 1, 0, 0,
                       (void*)zem_font_get_font_height);

    scm_c_export("load-font", "get-font-height", NULL);
}

void init_font_api()
{
    scm_c_define_module("zem api font", zem_api_font_init, nullptr);
}

} // namespace zem
