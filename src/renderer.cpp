#include "renderer.h"

#include <cassert>
#include <codecvt>
#include <locale>
#include <stack>
#include <string>
#include <vector>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

namespace zem {

template <> Renderer* Singleton<Renderer>::singleton = nullptr;

static std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t>
    conv_utf8_utf32;

struct ClipRect {
    float x, y;
    float width, height;

    ClipRect() { memset(this, 0, sizeof(*this)); }

    bool operator==(const ClipRect& rhs) const
    {
        return x == rhs.x && y == rhs.y && width == rhs.width &&
               height == rhs.height;
    }
    bool operator!=(const ClipRect& rhs) const { return !(*this == rhs); }
};

struct DrawCommand {
    unsigned int tex_id;
    GLSLShaderProgram* program;
    ClipRect clip_rect;
    size_t elem_offset;
    size_t elem_count;

    DrawCommand() : tex_id(0), program(nullptr), elem_offset(0), elem_count(0)
    {}
};

class CommandBuffer {
public:
    CommandBuffer() { reset(); }

    const std::vector<DrawCommand>& get_commands() const { return commands; }

    const std::vector<float>& get_vertex_buffer() const
    {
        return vertex_buffer;
    }
    const std::vector<int>& get_index_buffer() const { return index_buffer; }

    void reset()
    {
        current_tex_id = 0;
        current_program = nullptr;
        vertex_buffer.resize(0);
        vertex_count = 0;
        index_buffer.resize(0);
        commands.resize(0);
        while (!clip_rect_stack.empty())
            clip_rect_stack.pop();

        commands.emplace_back();
    }

    void add_draw_command()
    {
        DrawCommand cmd;
        cmd.tex_id = current_tex_id;
        cmd.program = current_program;
        cmd.elem_offset = index_buffer.size();

        if (!clip_rect_stack.empty()) cmd.clip_rect = clip_rect_stack.top();

        commands.push_back(cmd);
    }

    void set_texture(unsigned int tex_id)
    {
        auto& curr_cmd = commands.back();

        current_tex_id = tex_id;

        if (curr_cmd.elem_count > 0 && curr_cmd.tex_id != tex_id) {
            add_draw_command();
            return;
        }

        curr_cmd.tex_id = tex_id;
    }

    void set_program(GLSLShaderProgram* program)
    {
        auto& curr_cmd = commands.back();

        current_program = program;

        if (curr_cmd.elem_count > 0 && curr_cmd.program != nullptr &&
            curr_cmd.program != program) {
            add_draw_command();
            return;
        }

        curr_cmd.program = program;
    }

    void push_clip_rect(const Vector2& pos, const Vector2& size)
    {
        ClipRect cr;

        cr.x = pos.x;
        cr.y = pos.y;
        cr.width = size.x;
        cr.height = size.y;

        auto& curr_cmd = commands.back();

        clip_rect_stack.push(cr);

        if (curr_cmd.elem_count > 0 && curr_cmd.clip_rect.width > 0 &&
            curr_cmd.clip_rect.height > 0 && curr_cmd.clip_rect != cr) {
            add_draw_command();
            return;
        }

        curr_cmd.clip_rect = cr;
    }

    void pop_clip_rect()
    {
        auto& curr_cmd = commands.back();

        clip_rect_stack.pop();
        auto& cr = clip_rect_stack.top();

        if (curr_cmd.elem_count > 0 && curr_cmd.clip_rect.width > 0 &&
            curr_cmd.clip_rect.height > 0 && curr_cmd.clip_rect != cr) {
            add_draw_command();
            return;
        }

        curr_cmd.clip_rect = cr;
    }

    void add_rect(const Vector2& p_min, const Vector2& p_max,
                  const ColorValue& color)
    {
        auto& curr_cmd = commands.back();

        size_t idx = vertex_count;
        Vector2 v1(p_min), v2(p_max.x, p_min.y), v3(p_max),
            v4(p_min.x, p_max.y);
        Vector2 uv;

        index_buffer.push_back(idx);
        index_buffer.push_back(idx + 1);
        index_buffer.push_back(idx + 2);
        index_buffer.push_back(idx);
        index_buffer.push_back(idx + 2);
        index_buffer.push_back(idx + 3);

        append_vec2(v1);
        append_vec2(uv);
        append_color(color);
        append_vec2(v2);
        append_vec2(uv);
        append_color(color);
        append_vec2(v3);
        append_vec2(uv);
        append_color(color);
        append_vec2(v4);
        append_vec2(uv);
        append_color(color);

        vertex_count += 4;

        curr_cmd.elem_count += 6;
    }

    void add_glyph(const Vector2& pos, const GlyphInfo& glyph,
                   const ColorValue& color)
    {
        auto& curr_cmd = commands.back();

        Vector2 p_min = pos + Vector2{glyph.x0, glyph.y0};
        Vector2 p_max = pos + Vector2{glyph.x1, glyph.y1};
        Vector2 uv_min = Vector2{glyph.u0, glyph.v0};
        Vector2 uv_max = Vector2{glyph.u1, glyph.v1};

        size_t idx = vertex_count;
        Vector2 v1(p_min), v2(p_max.x, p_min.y), v3(p_max),
            v4(p_min.x, p_max.y);
        Vector2 uv1(uv_min), uv2(uv_max.x, uv_min.y), uv3(uv_max),
            uv4(uv_min.x, uv_max.y);

        index_buffer.push_back(idx);
        index_buffer.push_back(idx + 1);
        index_buffer.push_back(idx + 2);
        index_buffer.push_back(idx);
        index_buffer.push_back(idx + 2);
        index_buffer.push_back(idx + 3);

        append_vec2(v1);
        append_vec2(uv1);
        append_color(color);
        append_vec2(v2);
        append_vec2(uv2);
        append_color(color);
        append_vec2(v3);
        append_vec2(uv3);
        append_color(color);
        append_vec2(v4);
        append_vec2(uv4);
        append_color(color);

        vertex_count += 4;

        curr_cmd.elem_count += 6;
    }

private:
    std::vector<float> vertex_buffer;
    size_t vertex_count;
    std::vector<int> index_buffer;
    GLSLShaderProgram* current_program;
    unsigned int current_tex_id;
    std::stack<ClipRect> clip_rect_stack;

    std::vector<DrawCommand> commands;

    int append_vec2(const Vector2& v)
    {
        vertex_buffer.push_back(v.x);
        vertex_buffer.push_back(v.y);
        return 2;
    }

    int append_color(const ColorValue& color)
    {
        vertex_buffer.push_back(0.0f);
        *reinterpret_cast<unsigned int*>(&vertex_buffer.back()) =
            color.as_u32();
        return 1;
    }
};

Renderer::Renderer()
{
    command_buffer = std::make_unique<CommandBuffer>();
    init_resources();
}

void Renderer::init_resources()
{
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);
    glGenBuffers(1, &VBO);
    glGenBuffers(1, &EBO);

    const std::string vertex_shader =
        "#version 330 core\n"
        "layout (location = 0) in vec2 aPosition;\n"
        "layout (location = 1) in vec2 aTexcoord;\n"
        "layout (location = 2) in vec4 aColor;\n"
        "uniform mat4 uProjection;\n"
        "out vec2 vTexcoord;\n"
        "out vec4 vColor;\n"
        "void main()\n"
        "{\n"
        "    vTexcoord = aTexcoord;\n"
        "    vColor = aColor;\n"
        "    gl_Position = uProjection * vec4(aPosition, 0, 1);\n"
        "}\n";

    const std::string frag_shader_color = "#version 330 core\n"
                                          "in vec2 vTexcoord;\n"
                                          "in vec4 vColor;\n"
                                          "out vec4 out_color;\n"
                                          "void main()\n"
                                          "{\n"
                                          "    out_color = vColor;\n"
                                          "}\n";
    const std::string frag_shader_texcolor =
        "#version 330 core\n"
        "in vec2 vTexcoord;\n"
        "in vec4 vColor;\n"
        "uniform sampler2D uTexture;\n"
        "out vec4 out_color;\n"
        "void main()\n"
        "{\n"
        "    out_color = vColor * texture(uTexture, vTexcoord.st);\n"
        "}\n";

    program_color =
        std::make_unique<GLSLShaderProgram>(vertex_shader, frag_shader_color);
    assert(program_color->is_valid());

    program_texcolor = std::make_unique<GLSLShaderProgram>(
        vertex_shader, frag_shader_texcolor);
    assert(program_texcolor->is_valid());
}

void Renderer::add_rect(const Vector2& p_min, const Vector2& p_max,
                        const ColorValue& color)
{
    command_buffer->set_program(program_color.get());
    command_buffer->add_rect(p_min, p_max, color);
}

Vector2 Renderer::add_text(Font* font, const Vector2& pos,
                           const std::string& text, const ColorValue& color,
                           float max_width)
{
    Vector2 p = pos;
    std::u32string codepoints = conv_utf8_utf32.from_bytes(text);

    command_buffer->set_program(program_texcolor.get());

    for (auto&& codepoint : codepoints) {
        auto [glyph, tex_id] = font->find_glyph((uint32_t)codepoint);

        if (!tex_id) continue;

        if (max_width != 0 && p.x + glyph.x_advance > max_width) {
            p.x = 0;
            p.y += font->get_height_pixels();
        }

        command_buffer->set_texture(tex_id);
        command_buffer->add_glyph(p, glyph, color);

        p.x += glyph.x_advance;
    }

    return p;
}

Vector2 Renderer::text_size_hint(Font* font, const std::string& text,
                                 float max_width)
{
    float cur_x = 0;
    Vector2 p{0, (float)font->get_height_pixels()};
    std::u32string codepoints = conv_utf8_utf32.from_bytes(text);

    for (auto&& codepoint : codepoints) {
        auto [glyph, tex_id] = font->find_glyph((uint32_t)codepoint);

        if (!tex_id) continue;

        if (max_width != 0 && cur_x + glyph.x_advance > max_width) {
            p.x = std::max(p.x, cur_x);
            cur_x = 0;
            p.y += font->get_height_pixels();
        }

        cur_x += glyph.x_advance;
    }
    p.x = std::max(p.x, cur_x);

    return p;
}

void Renderer::setup_state()
{
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);

    glViewport(0, 0, display_width, display_height);

    glm::mat4 projection = glm::ortho(0.0f, (float)display_width,
                                      (float)display_height, 0.0f, -1.0f, 1.0f);
    program_color->bind();
    program_color->uniform("uProjection", 1, false, glm::value_ptr(projection));
    program_color->unbind();

    program_texcolor->bind();
    program_texcolor->uniform("uProjection", 1, false,
                              glm::value_ptr(projection));
    program_texcolor->uniform("uTexture", 0);
    program_texcolor->unbind();

    glBindVertexArray(VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat),
                          (GLvoid*)0);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat),
                          (GLvoid*)(sizeof(GLfloat) * 2));
    glVertexAttribPointer(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, 5 * sizeof(GLfloat),
                          (GLvoid*)(sizeof(GLfloat) * 4));
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
}

void Renderer::push_clip_rect(const Vector2& pos, const Vector2& size)
{
    command_buffer->push_clip_rect(pos, size);
}

void Renderer::pop_clip_rect() { command_buffer->pop_clip_rect(); }

void Renderer::begin_frame()
{
    command_buffer->reset();
    command_buffer->push_clip_rect(
        {0, 0}, {(float)display_width, (float)display_height});
}

void Renderer::end_frame()
{
    setup_state();

    const auto& vertex_buffer = command_buffer->get_vertex_buffer();
    const auto& index_buffer = command_buffer->get_index_buffer();

    glBufferData(GL_ARRAY_BUFFER,
                 (GLsizeiptr)vertex_buffer.size() * sizeof(float),
                 (const GLvoid*)&vertex_buffer[0], GL_STREAM_DRAW);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER,
                 (GLsizeiptr)index_buffer.size() * sizeof(int),
                 (const GLvoid*)&index_buffer[0], GL_STREAM_DRAW);

    glActiveTexture(GL_TEXTURE0);

    for (const auto& cmd : command_buffer->get_commands()) {
        if (!cmd.elem_count) continue;

        cmd.program->bind();

        auto& cr = cmd.clip_rect;
        glScissor((int)cr.x, (int)cr.y, (int)cr.width, (int)cr.height);

        glBindTexture(GL_TEXTURE_2D, (GLuint)cmd.tex_id);

        glDrawElementsBaseVertex(
            GL_TRIANGLES, (GLsizei)cmd.elem_count, GL_UNSIGNED_INT,
            (void*)(intptr_t)(cmd.elem_offset * sizeof(int)), 0);
    }
}

extern void init_renderer_api();
extern void init_font_api();

void init_renderer()
{
    (void)new Renderer();

    init_renderer_api();
    init_font_api();
}

} // namespace zem
