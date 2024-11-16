#include "shader_program.h"

#include "spdlog/spdlog.h"

namespace zem {

BaseShaderProgram::BaseShaderProgram() : _valid(false) {}

void BaseShaderProgram::uniform(UniformID id, float v0, float v1, float v2,
                                float v3)
{
    Binding b = get_uniform_binding(id);
    uniform_(b, v0, v1, v2, v3);
}

void BaseShaderProgram::uniform(UniformID id, int v0, int v1, int v2, int v3)
{
    Binding b = get_uniform_binding(id);
    uniform_(b, v0, v1, v2, v3);
}

void BaseShaderProgram::uniform(UniformID id, GLsizei count,
                                GLboolean transpose, const GLfloat* mat)
{
    Binding b = get_uniform_binding(id);
    uniform_(b, count, transpose, mat);
}

GLSLShaderProgram::GLSLShaderProgram(const std::string& vertex_code,
                                     const std::string& fragment_code)
{
    vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    program = 0;

    if (!compile(vertex_shader, vertex_code)) return;
    if (!compile(fragment_shader, fragment_code)) return;

    if (!link()) return;

    _valid = true;
}

GLSLShaderProgram::~GLSLShaderProgram()
{
    _valid = false;
    if (program) {
        glDeleteProgram(program);
    }
    program = 0;

    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
}

void GLSLShaderProgram::bind() { glUseProgram(program); }

void GLSLShaderProgram::unbind() { glUseProgram(0); }

bool GLSLShaderProgram::compile(GLuint shader, const std::string& code)
{
    const char* source = code.c_str();
    glShaderSource(shader, 1, &source, NULL);
    glCompileShader(shader);

    GLint success;
    GLchar info[512];
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);

    if (!success) {
        glGetShaderInfoLog(shader, 512, NULL, info);
        spdlog::error("Shader compilation failed: {}", info);
    }

    return success;
}

bool GLSLShaderProgram::link()
{
    program = glCreateProgram();

    glAttachShader(program, vertex_shader);
    glAttachShader(program, fragment_shader);
    glLinkProgram(program);

    GLint success;
    GLchar info[512];
    glGetProgramiv(program, GL_LINK_STATUS, &success);

    if (!success) {
        glGetProgramInfoLog(program, 512, NULL, info);
        spdlog::error("Shader program linking failed: {}", info);
        return false;
    }

    uniforms.clear();

    bind();
    int num_uniforms;
    glGetProgramiv(program, GL_ACTIVE_UNIFORMS, &num_uniforms);

    for (int i = 0; i < num_uniforms; i++) {
        char name[256] = "";
        int name_length;
        GLint size;
        GLenum type;
        glGetActiveUniform(program, i, sizeof(name), &name_length, &size, &type,
                           name);

        GLint loc = glGetUniformLocation(program, name);

        InternString uni_name(name);
        uniforms[uni_name] = std::make_pair(loc, type);
    }
    unbind();

    return success;
}

BaseShaderProgram::Binding GLSLShaderProgram::get_uniform_binding(UniformID id)
{
    auto iter = uniforms.find(id);
    if (iter == uniforms.end()) {
        return Binding();
    }

    return Binding((int)iter->second.first, (int)iter->second.second);
}

void GLSLShaderProgram::uniform_(const BaseShaderProgram::Binding& b, float v0,
                                 float v1, float v2, float v3)
{
    int id = b.first;
    if (id != -1) {
        GLenum type = (GLenum)b.second;

        switch (type) {
        case GL_FLOAT:
            glUniform1f(id, v0);
        case GL_FLOAT_VEC2:
            glUniform2f(id, v0, v1);
            break;
        case GL_FLOAT_VEC3:
            glUniform3f(id, v0, v1, v2);
            break;
        case GL_FLOAT_VEC4:
            glUniform4f(id, v0, v1, v2, v3);
            break;
        default:
            spdlog::error("Mismatch uniform binding {}", b.first);
            break;
        }
    }
}

void GLSLShaderProgram::uniform_(const BaseShaderProgram::Binding& b, int v0,
                                 int v1, int v2, int v3)
{
    int id = b.first;
    if (id != -1) {
        GLenum type = (GLenum)b.second;

        switch (type) {
        case GL_INT:
        case GL_SAMPLER_2D:
            glUniform1i(id, v0);
            break;
        case GL_INT_VEC2:
            glUniform2i(id, v0, v1);
            break;
        case GL_INT_VEC3:
            glUniform3i(id, v0, v1, v2);
            break;
        case GL_INT_VEC4:
            glUniform4i(id, v0, v1, v2, v3);
            break;
        default:
            spdlog::error("Mismatch uniform binding {}", b.first);
            break;
        }
    }
}

void GLSLShaderProgram::uniform_(const Binding& b, GLsizei count,
                                 GLboolean transpose, const GLfloat* mat)
{
    int id = b.first;

    if (id != -1) {
        GLenum type = (GLenum)b.second;

        switch (type) {
        case GL_FLOAT_MAT4:
            glUniformMatrix4fv(id, count, transpose, mat);
            break;
        default:
            spdlog::error("Mismatch uniform binding {}", b.first);
            break;
        }
    }
}

} // namespace zem
