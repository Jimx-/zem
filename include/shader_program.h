#ifndef _ZEM_SHADER_PROGRAM_H
#define _ZEM_SHADER_PROGRAM_H

#include "intern_string.h"

#include <glad/glad.h>
#include <map>
#include <string>

namespace zem {

class BaseShaderProgram {
public:
    typedef InternString UniformID;

    struct Binding {
        int first, second;

        Binding(int first = -1, int second = -1) : first(first), second(second)
        {}
    };

    virtual ~BaseShaderProgram() {}

    virtual void bind() = 0;
    virtual void unbind() = 0;

    virtual GLuint get_program() const = 0;

    bool is_valid() const { return _valid; }

    void uniform(UniformID id, float v0, float v1 = 0.0f, float v2 = 0.0f,
                 float v3 = 0.0f);
    void uniform(UniformID id, int v0, int v1 = 0, int v2 = 0, int v3 = 0);
    void uniform(UniformID id, GLsizei count, GLboolean transpose,
                 const GLfloat* mat);

protected:
    BaseShaderProgram();
    bool _valid;

    virtual Binding get_uniform_binding(UniformID id) = 0;

    virtual void uniform_(const Binding& b, float v0, float v1, float v2,
                          float v3) = 0;
    virtual void uniform_(const Binding& b, int v0, int v1, int v2, int v3) = 0;
    virtual void uniform_(const Binding& b, GLsizei count, GLboolean transpose,
                          const GLfloat* mat) = 0;
};

class GLSLShaderProgram : public BaseShaderProgram {
public:
    GLSLShaderProgram(const std::string& vertex_code,
                      const std::string& fragment_code);
    virtual ~GLSLShaderProgram();

    virtual void bind();
    virtual void unbind();

    virtual GLuint get_program() const { return program; }

private:
    std::string vertex_code;
    std::string fragment_code;

    GLuint program;
    GLuint vertex_shader;
    GLuint fragment_shader;

    std::map<InternString, std::pair<int, GLenum>> uniforms;

    bool compile(GLuint shader, const std::string& code);
    bool link();

    virtual Binding get_uniform_binding(UniformID id);
    virtual void uniform_(const Binding& b, float v0, float v1, float v2,
                          float v3);
    virtual void uniform_(const Binding& b, int v0, int v1, int v2, int v3);
    virtual void uniform_(const Binding& b, GLsizei count, GLboolean transpose,
                          const GLfloat* mat);
};

} // namespace zem

#endif
