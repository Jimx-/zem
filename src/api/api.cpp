namespace zem {

extern void init_renderer_api();
extern void init_font_api();

void init_api()
{
    init_renderer_api();
    init_font_api();
}

} // namespace zem
