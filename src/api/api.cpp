namespace zem {

extern void init_renderer_api();
extern void init_font_api();
extern void init_tree_sitter_api();

void init_api()
{
    init_renderer_api();
    init_font_api();
    init_tree_sitter_api();
}

} // namespace zem
