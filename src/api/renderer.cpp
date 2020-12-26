#include "renderer.h"

#include <libguile.h>

namespace zem {

static Vector2 extract_vec2(SCM val)
{
    float x = scm_to_double(scm_car(val));
    float y = scm_to_double(scm_cdr(val));
    return Vector2{x, y};
}

static ColorValue extract_color(SCM val)
{
    return ColorValue::from_u32(scm_to_uint32(val));
}

static SCM zem_renderer_add_rect(SCM s_pos, SCM s_size, SCM s_color)
{
    Vector2 pos = extract_vec2(s_pos);
    Vector2 size = extract_vec2(s_size);
    ColorValue color = extract_color(s_color);

    RENDERER.add_rect(pos, pos + size, color);

    return SCM_UNSPECIFIED;
}

static SCM zem_renderer_add_text(SCM s_font, SCM s_pos, SCM s_text, SCM s_color,
                                 SCM s_max_width)
{
    Font* font = (Font*)scm_foreign_object_ref(s_font, 0);
    Vector2 pos = extract_vec2(s_pos), new_pos;
    ColorValue color = extract_color(s_color);
    std::unique_ptr<char[]> text;
    float max_width = 0;

    text.reset(scm_to_locale_string(s_text));
    if (s_max_width != SCM_UNDEFINED) max_width = scm_to_double(s_max_width);

    new_pos =
        RENDERER.add_text(font, pos, std::string(text.get()), color, max_width);

    return scm_cons(scm_from_double(new_pos.x), scm_from_double(new_pos.y));
}

static SCM zem_renderer_text_size_hint(SCM s_font, SCM s_text, SCM s_max_width)
{
    Font* font = (Font*)scm_foreign_object_ref(s_font, 0);
    Vector2 size;
    std::unique_ptr<char[]> text;
    float max_width = 0;

    text.reset(scm_to_locale_string(s_text));
    if (s_max_width != SCM_UNDEFINED) max_width = scm_to_double(s_max_width);

    size = RENDERER.text_size_hint(font, std::string(text.get()), max_width);

    return scm_cons(scm_from_double(size.x), scm_from_double(size.y));
}

static SCM zem_renderer_push_clip_rect(SCM s_pos, SCM s_size)
{
    Vector2 pos = extract_vec2(s_pos);
    Vector2 size = extract_vec2(s_size);

    RENDERER.push_clip_rect(pos, size);

    return SCM_UNSPECIFIED;
}

static SCM zem_renderer_pop_clip_rect()
{
    RENDERER.pop_clip_rect();
    return SCM_UNSPECIFIED;
}

void zem_api_renderer_init(void* data)
{
    scm_c_define_gsubr("add-rect", 3, 0, 0, (void*)zem_renderer_add_rect);
    scm_c_define_gsubr("add-text", 4, 1, 0, (void*)zem_renderer_add_text);
    scm_c_define_gsubr("text-size-hint", 2, 1, 0,
                       (void*)zem_renderer_text_size_hint);
    scm_c_define_gsubr("push-clip-rect", 2, 0, 0,
                       (void*)zem_renderer_push_clip_rect);
    scm_c_define_gsubr("pop-clip-rect", 0, 0, 0,
                       (void*)zem_renderer_pop_clip_rect);
    scm_c_export("add-rect", "add-text", "text-size-hint", "push-clip-rect",
                 "pop-clip-rect", NULL);
}

void init_renderer_api()
{
    scm_c_define_module("zem api renderer", zem_api_renderer_init, nullptr);
}

} // namespace zem
