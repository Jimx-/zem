#ifndef _ZEM_RENDERER_H
#define _ZEM_RENDERER_H

#include "color_value.h"
#include "font.h"
#include "shader_program.h"
#include "singleton.h"
#include "vector2.h"

#include <iconv.h>
#include <memory>

namespace zem {

class CommandBuffer;

class Renderer : public Singleton<Renderer> {
public:
    Renderer();

    void set_display_size(unsigned int width, unsigned int height)
    {
        display_width = width;
        display_height = height;
    }

    void add_rect(const Vector2& p_min, const Vector2& p_max,
                  const ColorValue& color);
    Vector2 add_text(Font* font, const Vector2& pos, const std::string& text,
                     const ColorValue& color, float max_width = 0);

    Vector2 text_size_hint(Font* font, const std::string& text,
                           float max_width = 0);

    void push_clip_rect(const Vector2& pos, const Vector2& size);
    void pop_clip_rect();

    void begin_frame();
    void end_frame();

private:
    unsigned int display_width;
    unsigned int display_height;

    std::unique_ptr<GLSLShaderProgram> program_color;
    std::unique_ptr<GLSLShaderProgram> program_texcolor;

    std::unique_ptr<CommandBuffer> command_buffer;

    GLuint VAO, VBO, EBO;

    void init_resources();

    void setup_state();
};

void init_renderer();

} // namespace zem

#define RENDERER (zem::Renderer::get_singleton())

#endif
