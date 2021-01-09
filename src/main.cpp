#include <glad/glad.h>

#include <GLFW/glfw3.h>
#include <libguile.h>

#include <chrono>

#include <emacsy.h>

#include "font.h"
#include "renderer.h"

static SCM g_root_view_module;
static SCM g_root_view;

namespace zem {
extern void init_api();
}

static void queue_redraw()
{
    static SCM func = SCM_VARIABLE_REF(
        scm_c_module_lookup(g_root_view_module, "queue-redraw"));
    scm_call_0(func);
}

static void window_size_callback(GLFWwindow* window, int width, int height)
{
    static SCM resize_func = SCM_VARIABLE_REF(
        scm_c_module_lookup(g_root_view_module, "resize-root-view"));

    RENDERER.set_display_size(width, height);

    scm_call_3(resize_func, g_root_view, scm_from_double((double)width),
               scm_from_double((double)height));
}

static void key_callback(GLFWwindow* window, int glfw_key, int scancode,
                         int action, int mods)
{
    unsigned char key = 0;
    int mod_flags = 0;

    if (action == GLFW_PRESS) {
        if (glfw_key >= GLFW_KEY_A && glfw_key <= GLFW_KEY_Z)
            key = (unsigned char)(glfw_key + ('a' - 'A'));
        else if (glfw_key < 256)
            key = (unsigned char)glfw_key;
        else {
            switch (glfw_key) {
            case GLFW_KEY_TAB:
                key = '\t';
                break;
            case GLFW_KEY_ENTER:
                key = '\n';
                break;
            case GLFW_KEY_ESCAPE:
                key = '\033';
                break;
            case GLFW_KEY_BACKSPACE:
                key = '\x7f';
                break;
            }
        }

        if (mods & GLFW_MOD_CONTROL) mod_flags |= EMACSY_MODKEY_CONTROL;
        if (mods & GLFW_MOD_SHIFT) mod_flags |= EMACSY_MODKEY_SHIFT;
        if (mods & GLFW_MOD_ALT) mod_flags |= EMACSY_MODKEY_META;
        if (mods & GLFW_MOD_SUPER) mod_flags |= EMACSY_MODKEY_SUPER;

        if (key) {
            emacsy_key_event(key, mod_flags);
            queue_redraw();
        }
    }
}

static void cursor_position_callback(GLFWwindow* window, double xpos,
                                     double ypos)
{
    scm_call_3(SCM_VARIABLE_REF(scm_c_module_lookup(
                   g_root_view_module, "view:mouse-position-callback")),
               g_root_view, scm_from_double(xpos), scm_from_double(ypos));
    queue_redraw();
}

void mouse_button_callback(GLFWwindow* window, int glfw_button, int action,
                           int mods)
{
    int button;
    double xpos, ypos;

    switch (glfw_button) {
    case GLFW_MOUSE_BUTTON_LEFT:
        button = 1;
        break;
    case GLFW_MOUSE_BUTTON_MIDDLE:
        button = 2;
        break;
    case GLFW_MOUSE_BUTTON_RIGHT:
        button = 3;
        break;
    default:
        button = 0;
        break;
    }

    glfwGetCursorPos(window, &xpos, &ypos);

    if (action == GLFW_PRESS) {
        scm_call_4(SCM_VARIABLE_REF(scm_c_module_lookup(
                       g_root_view_module, "view:mouse-press-callback")),
                   g_root_view, scm_from_int(button), scm_from_double(xpos),
                   scm_from_double(ypos));
    }
}

static void scroll_callback(GLFWwindow* window, double xoffset, double yoffset)
{
    scm_call_2(SCM_VARIABLE_REF(scm_c_module_lookup(
                   g_root_view_module, "view:mouse-scroll-callback")),
               g_root_view, scm_from_double(yoffset));
    queue_redraw();
}

static void main_loop(GLFWwindow* window)
{
    SCM view_draw;
    SCM update_root_view;
    std::chrono::steady_clock::time_point last_time;
    std::chrono::steady_clock::time_point cur_time;
    double delta;

    view_draw = scm_c_module_lookup(g_root_view_module, "view:draw");
    update_root_view =
        scm_c_module_lookup(g_root_view_module, "update-root-view");

    last_time = std::chrono::steady_clock::now();

    while (glfwWindowShouldClose(window) == false) {
        cur_time = std::chrono::steady_clock::now();
        delta = std::chrono::duration_cast<std::chrono::microseconds>(cur_time -
                                                                      last_time)
                    .count() /
                1000000.0;
        last_time = cur_time;

        if (emacsy_tick() & EMACSY_QUIT_APPLICATION_P) {
            glfwSetWindowShouldClose(window, true);
        }

        SCM need_redraw = scm_call_2(SCM_VARIABLE_REF(update_root_view),
                                     g_root_view, scm_from_double(delta));

        if (scm_to_bool(need_redraw)) {
            RENDERER.begin_frame();

            glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

            scm_call_1(SCM_VARIABLE_REF(view_draw), g_root_view);

            RENDERER.end_frame();

            glfwSwapBuffers(window);
        }

        glfwPollEvents();
    }

    emacsy_terminate();
}

static void inner_main(void* data, int argc, char** argv)
{
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    GLFWwindow* window = glfwCreateWindow(800, 600, "ZEM", nullptr, nullptr);
    if (window == nullptr) {
        glfwTerminate();
        return;
    }

    glfwMakeContextCurrent(window);
    gladLoadGL();

    zem::init_renderer();
    zem::init_api();

    emacsy_initialize(EMACSY_INTERACTIVE);

    g_root_view_module = scm_c_resolve_module("zem ui root-view");
    g_root_view = scm_call_0(SCM_VARIABLE_REF(
        scm_c_module_lookup(g_root_view_module, "make-root-view")));

    window_size_callback(window, 800, 600);
    glfwSetWindowSizeCallback(window, window_size_callback);
    glfwSetKeyCallback(window, key_callback);
    glfwSetCursorPosCallback(window, cursor_position_callback);
    glfwSetMouseButtonCallback(window, mouse_button_callback);
    glfwSetScrollCallback(window, scroll_callback);

    main_loop(window);
}

int main(int argc, char** argv)
{
    scm_boot_guile(argc, argv, inner_main, 0);
    return 0;
}
